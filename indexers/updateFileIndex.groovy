/**
 * Main script for running Mloc
 *
 * @author <a href="mailto:martin.lebeda@gmail.com">Martin Lebeda</a>
 * Date: 31.7.14
 */
@Grapes([
        @Grab(group = 'org.xerial', module = 'sqlite-jdbc', version = '3.8.11.2'),
        @GrabConfig(systemClassLoader = true)
])
import groovy.sql.Sql
import groovy.transform.CompileStatic
import groovy.transform.Field

import java.nio.file.Files
import java.nio.file.Paths
import java.text.Normalizer

@Field String base
@Field avfsSuffix = ['.zip', '.rar', '.tgz', '.tag.gz', '.tgz', '.tar.bz2', '.7z', '.jar']
@Field String jdbcUrl
@Field Sql sql
@Field String avfsMount
@Field String tagname = null
@Field int priority = 50
@Field String cmdname = 'exo-open'
@Field boolean reindex = true
@Field boolean gitSupport = false

@Field static String CMD_DELREC = "delrec"
@Field static String CMD_DELTAG = "deltag"
@Field static String CMD_FILE = "file"
@Field static String CMD_CMD = "cmd"
@Field static String CMD_PATH = "path"

def cli = new CliBuilder(usage: 'Mloc [options] arg...', header: 'Options:')
cli.help('print this message')
cli.b(longOpt: 'base', 'Path to database path', args: 1, argName: 'dbPath', required: true)
cli.t(longOpt: 'tag', 'Tag for scanned entries', args: 1, argName: 'tagname')
cli.i(longOpt: 'priority', 'Priority for scanned entries in results', args: 1, argName: 'priority')
cli.c(longOpt: 'cmd', 'Command for open scanned entries', args: 1, argName: 'cmdname')
cli.u(longOpt: 'update', 'Update index from source files in arg')
cli.p(longOpt: "path", "Simple recursive add paths in arg to index")
cli._(longOpt: 'avfs', 'Path to avfs mount', args: 1, argName: 'avfsPath')
cli._(longOpt: 'noreindex', 'ignore fulltext reindexation (for use in batch update)')
cli._(longOpt: 'git', 'if directory contain .git use "git ls-files" instead recursive direct listing')

def opt = cli.parse(args)
if (!opt) return
if (opt.h) cli.usage()

if (opt.b) {
    base = opt.b
}
// create database if not exists
if (!base) return "database is not specified"
println("Using DB: ${base}")
jdbcUrl = "jdbc:sqlite:${this.base}"
sql = Sql.newInstance(jdbcUrl, "org.sqlite.JDBC")
sql.execute("CREATE TABLE IF NOT EXISTS sources (id PRIMARY KEY, path, name, search, command, updated, tag, priority, trash, description)")
if (!isTableExists("sourcesSearch")) {
    sql.execute("CREATE VIRTUAL TABLE sourcesSearch USING FTS4(id, search)")
}
//sql.setAutoCommit(false)

// tag support
if (opt.t) {
    tagname = opt.t
}

// priority support
if (opt.i) {
    priority = Integer.parseInt(opt.i)
}

// cmd support
if (opt.c) {
    cmdname = opt.c
}

// ignore reindex
if (opt.'noreindex') {
    reindex = false
}

// support for git
gitSupport = opt.'git'

// AVFS - support for archives
if (opt.'avfs') {
    avfsMount = opt.'avfs'
} else {
    avfsMount = null
}

// run update from listing file
if (opt.u) {
    def cnt = 0
    opt.arguments().each { arg ->
        sql.withTransaction {
            println("Update from source: ${arg}.")
            new File(arg).eachLine { line ->
                cnt += processCommandLineInternal(line)
            }
        }
    }
    println("Totally processed ${opt.arguments().size()} ($cnt commands)")
    refreshFtIndex()
}

// simple index path recursively
if (opt.p) {
    def cnt = 0
    opt.arguments().each {
        println("recursive find ${it}...")
        cnt += processCommand("$CMD_DELREC ${it}")
        cnt += processCommand("$CMD_PATH ${it}")
    }
    println("Totally processed ${opt.arguments().size()} ($cnt commands)")
    refreshFtIndex()
}


sql.close()

def file = new File(base)
println(file.absolutePath)

//-------------------------------------------------------------------
def refreshFtIndex() {
    sql.execute("DELETE FROM sources WHERE trash = 1")

    if (reindex) {
        sql.withTransaction {
            sql.execute("DELETE FROM sourcesSearch WHERE id NOT IN (SELECT id FROM sources)")
            sql.execute("INSERT INTO sourcesSearch (id, search) SELECT id, search FROM sources WHERE id NOT IN (SELECT id FROM sourcesSearch)")
        }
        println("Reindexation completed")
    } else {
        println("Reindexation skipped")
    }
}

@CompileStatic
int processCommand(String line) {
    def cnt = 0
    sql.withTransaction {
        cnt = processCommandLineInternal(line)
        println("Processed ${line} (${cnt} commands)")
    }
    cnt
}

@CompileStatic
int processCommandLineInternal(String line) {
    def cnt = 0
    if (line =~ /^[^#]/) {
        // skip comments

        // split into command and value
        def split = line.split(" ", 2)
        def command = split[0]
        String value = null
        if (split.length > 1) {
            value = split[1].trim()
        }

        switch (command) {
            case CMD_DELREC:
                if (value) {
                    deletePathFromIndex(value)
                    cnt++
                }
                break

            case CMD_DELTAG:
                deleteTagFromIndex()
                cnt++
                break

            case CMD_FILE:
                if (value) {
                    addFileToIndex(value)
                    cnt++
                }
                break

            case CMD_CMD:
                if (value) {
                    addCmdToIndex(value)
                    cnt++
                }
                break

        // TODO Lebeda - support for parsing icewm/blackbox menu

            case CMD_PATH:
                if (value) {
                    def path = trimLastPathSeparator(value)

                    def filePath = new File(path);
                    try {
                        if (gitSupport && Files.exists(Paths.get(filePath.absolutePath, '.git'))) {
                            def processBuilder = new ProcessBuilder(["git", "ls-files"])
                            processBuilder.redirectErrorStream(true)
                            processBuilder.directory(filePath)
                            def process = processBuilder.start()
                            process.inputStream.eachLine { String gitFileName ->
                                def absolutePath = Paths.get(filePath.absolutePath, gitFileName).toFile().absolutePath
//                                println absolutePath
                                addFileToIndex(absolutePath)
                                // TODO Lebeda - support for batch add to sql
                                cnt++
                            }
                            process.waitFor()

                        } else {
                            filePath.eachFile { File f ->
                                // TODO Lebeda - support for include/exclude

//                            println(f.absolutePath)
                                addFileToIndex(f.absolutePath)
                                // add directories in recursive task
                                if (f.isDirectory()) {
                                    cnt += processCommandLineInternal("$CMD_PATH ${f.absolutePath}")
                                }

                                // TODO Lebeda - support for symlink to directory

                                // TODO Lebeda - support for prepared .lst files
                                if (avfsMount && isAvfsSupported(f.absolutePath) && !f.absolutePath.contains(avfsMount)) {
                                    def avfsPath = getAvfsPath(avfsMount, f.absolutePath)
                                    long changed = f.lastModified()
                                    long indexed = (Long) (sql.firstRow("select min(updated) as indexed from sources WHERE path like ${avfsPath + "%"}").indexed ?: 0)
                                    if (changed > indexed) {
                                        println("index from avfs: ${avfsPath}")
                                        cnt += processCommandLineInternal("$CMD_DELREC ${avfsPath}")
                                        cnt += processCommandLineInternal("$CMD_PATH ${avfsPath}")
                                    } else {
                                        println("index from cache: ${avfsPath}")
                                        sql.execute("update sources set trash = null where path like ${avfsPath + "%"}")
                                    }
                                }
                                cnt++
                            }
                        }
                    } catch (Exception e) {
                        println("Cannot explore path: ${path}\n${e}");
                    }

                }
                break;

            default:
                println("Unrecognized command: ${command}")
        }
    }
    cnt
}

// TODO - JavaDoc - Lebeda
@CompileStatic
String trimLastPathSeparator(String path) {
    path.replaceAll(/[\\\\/]$/, "")
}

// TODO - JavaDoc - Lebeda
@CompileStatic
boolean isAvfsSupported(String value) {
    if (avfsSuffix) {
        return avfsSuffix.any { String it ->
            value.toLowerCase().endsWith(it.toLowerCase())
        }
    } else {
        return false
    }
}

// TODO - JavaDoc - Lebeda
@CompileStatic
String getAvfsPath(final String avfsMount, final String path) {
    return avfsMount + path + '#';
}

/**
 * Add file to index - implementation of command 'add'.
 *
 * @param value full path to file
 * @param writer instance of writer
 */
@CompileStatic
def addFileToIndex(String value) {
    def file = new File(value)

    // TODO podpora pro .desktop soubory
    if (file.name.endsWith(".desktop")) {

        def lines = file.readLines()

        String name = lines.grep(~/^Name=.*/)?.join("")?.replaceFirst(/^Name=/, '')
        String nameCz = lines.grep(~/^Name\[cz\]=.*/)?.join("")?.replaceFirst(/^Name.*?=/, '')
        String comment = lines.grep(~/^Comment=.*/)?.join("")?.replaceFirst(/^Comment=/, '')
        String commentCz = lines.grep(~/^Comment\[cz\]=.*/)?.join("")?.replaceFirst(/^Comment.*?=/, '')
        String keywords = lines.grep(~/^Keywords=.*/)?.join("")?.replaceFirst(/^Keywords=/, '')

        String description = [name, nameCz, comment, commentCz, keywords, file.name].join(" ").trim()
        addValueToDB(file.absolutePath, name, cmdname, description)
    } else { // obecné zařazení souboru
        addValueToDB(file.absolutePath, file.name, cmdname)
    }

}

def void addValueToDB(String path, String name, String command, String description = '') {
    def src = sql.dataSet("sources")

    def seachPattern = tagname?.trim()?.toLowerCase() + " " + (name.toLowerCase().trim() + " " + description?.toLowerCase().trim()).replaceAll(/[^A-Za-z0-9]/, " ")

    src.add(
            id: UUID.randomUUID(),
            path: path,
            name: name,
            search: removeDiacritics(seachPattern?.trim()),
            command: command,
            updated: new Date(),
            tag: tagname,
            priority: priority,
            description: description
    )
}

@CompileStatic
def addCmdToIndex(String value) {
    def split = value.split(/\|/, 4)
    String command = split[0].trim()
    String name = split[1]?.trim()
    String filename = split[2]?.trim()
    String description = (split.size() > 3 ? split[3]?.trim() : "")

    addValueToDB(filename, name, command, description)
}

/**
 * Delete path from index - implementation of command derec.
 * Delete path recursively from index.
 * Ex: /home/user -> delete /home/user/*
 *
 * @param value
 * @param writer
 */
@CompileStatic
def deletePathFromIndex(String value) {
    value = trimLastPathSeparator(value)
    // println("Deleting objects by query: ${value} " + getTagWhere())
    sql.execute("delete from sources where path = ${value} and trash is null " + getTagWhere())
    sql.execute("delete from sources where path like ${value + '/%'}  and trash is null " + getTagWhere())
}

// TODO - JavaDoc - Lebeda
def deleteTagFromIndex() {
    def s = "delete from sources where 1=1 " + getTagWhere()
    sql.execute(s)
    println("Deleting objects by tag: ${tagname}")
}

// TODO - JavaDoc - Lebeda
String getTagWhere() {
    def tagWhere = tagname ? " and tag = '${tagname}' " : " and tag is null "
    return tagWhere?.trim()
}

// TODO - JavaDoc - Lebeda
@CompileStatic
String removeDiacritics(String s) {
    return Normalizer.normalize(s, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
}

// TODO - JavaDoc - Lebeda
boolean isTableExists(String tableName) {
    sql.firstRow("SELECT COUNT(*) as cnt FROM sqlite_master WHERE type = 'table' AND name = ${tableName}").cnt > 0;
}

