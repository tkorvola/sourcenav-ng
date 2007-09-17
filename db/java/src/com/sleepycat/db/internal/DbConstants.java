/* DO NOT EDIT: automatically built by dist/s_java_const. */

package com.sleepycat.db.internal;

public interface DbConstants
{
    int DB_AFTER = 1;
    int DB_AGGRESSIVE = 0x0000001;
    int DB_APPEND = 2;
    int DB_ARCH_ABS = 0x001;
    int DB_ARCH_DATA = 0x002;
    int DB_ARCH_LOG = 0x004;
    int DB_ARCH_REMOVE = 0x008;
    int DB_AUTO_COMMIT = 0x02000000;
    int DB_BEFORE = 3;
    int DB_BTREE = 1;
    int DB_CDB_ALLDB = 0x00004000;
    int DB_CHKSUM = 0x00004000;
    int DB_CONSUME = 4;
    int DB_CONSUME_WAIT = 5;
    int DB_CREATE = 0x0000001;
    int DB_CURRENT = 6;
    int DB_DBT_MALLOC = 0x008;
    int DB_DBT_PARTIAL = 0x020;
    int DB_DBT_USERMEM = 0x100;
    int DB_DIRECT_DB = 0x00008000;
    int DB_DIRECT_LOG = 0x00010000;
    int DB_DSYNC_DB = 0x00020000;
    int DB_DSYNC_LOG = 0x00040000;
    int DB_DUP = 0x00008000;
    int DB_DUPSORT = 0x00010000;
    int DB_EID_BROADCAST = -1;
    int DB_EID_INVALID = -2;
    int DB_ENCRYPT = 0x00020000;
    int DB_ENCRYPT_AES = 0x0000001;
    int DB_EXCL = 0x0004000;
    int DB_FAST_STAT = 0x0000001;
    int DB_FIRST = 7;
    int DB_FLUSH = 0x001;
    int DB_FORCE = 0x0000004;
    int DB_FREELIST_ONLY = 0x00004000;
    int DB_FREE_SPACE = 0x00008000;
    int DB_GET_BOTH = 8;
    int DB_GET_BOTH_RANGE = 10;
    int DB_GET_RECNO = 11;
    int DB_HASH = 2;
    int DB_IGNORE_LEASE = 0x01000000;
    int DB_IMMUTABLE_KEY = 0x0004000;
    int DB_INIT_CDB = 0x0010000;
    int DB_INIT_LOCK = 0x0020000;
    int DB_INIT_LOG = 0x0040000;
    int DB_INIT_MPOOL = 0x0080000;
    int DB_INIT_REP = 0x0100000;
    int DB_INIT_TXN = 0x0200000;
    int DB_INORDER = 0x00040000;
    int DB_JOINENV = 0x0;
    int DB_JOIN_ITEM = 12;
    int DB_JOIN_NOSORT = 0x0000001;
    int DB_KEYEMPTY = -30997;
    int DB_KEYEXIST = -30996;
    int DB_KEYFIRST = 13;
    int DB_KEYLAST = 14;
    int DB_LAST = 15;
    int DB_LOCKDOWN = 0x0400000;
    int DB_LOCK_DEADLOCK = -30995;
    int DB_LOCK_DEFAULT = 1;
    int DB_LOCK_EXPIRE = 2;
    int DB_LOCK_GET = 1;
    int DB_LOCK_GET_TIMEOUT = 2;
    int DB_LOCK_IREAD = 5;
    int DB_LOCK_IWR = 6;
    int DB_LOCK_IWRITE = 4;
    int DB_LOCK_MAXLOCKS = 3;
    int DB_LOCK_MAXWRITE = 4;
    int DB_LOCK_MINLOCKS = 5;
    int DB_LOCK_MINWRITE = 6;
    int DB_LOCK_NOTGRANTED = -30994;
    int DB_LOCK_NOWAIT = 0x002;
    int DB_LOCK_OLDEST = 7;
    int DB_LOCK_PUT = 4;
    int DB_LOCK_PUT_ALL = 5;
    int DB_LOCK_PUT_OBJ = 6;
    int DB_LOCK_RANDOM = 8;
    int DB_LOCK_READ = 1;
    int DB_LOCK_TIMEOUT = 8;
    int DB_LOCK_WRITE = 2;
    int DB_LOCK_YOUNGEST = 9;
    int DB_LOG_AUTOREMOVE = 0x00080000;
    int DB_LOG_INMEMORY = 0x00100000;
    int DB_MPOOL_NOFILE = 0x001;
    int DB_MPOOL_UNLINK = 0x002;
    int DB_MULTIPLE = 0x10000000;
    int DB_MULTIPLE_KEY = 0x20000000;
    int DB_MULTIVERSION = 0x0000008;
    int DB_NEXT = 16;
    int DB_NEXT_DUP = 17;
    int DB_NEXT_NODUP = 18;
    int DB_NODUPDATA = 19;
    int DB_NOLOCKING = 0x00200000;
    int DB_NOMMAP = 0x0000010;
    int DB_NOORDERCHK = 0x0000002;
    int DB_NOOVERWRITE = 20;
    int DB_NOPANIC = 0x00400000;
    int DB_NOSERVER_HOME = -30991;
    int DB_NOSERVER_ID = -30990;
    int DB_NOSYNC = 21;
    int DB_NOTFOUND = -30989;
    int DB_ORDERCHKONLY = 0x0000004;
    int DB_OVERWRITE = 0x00800000;
    int DB_PANIC_ENVIRONMENT = 0x01000000;
    int DB_POSITION = 22;
    int DB_PREV = 23;
    int DB_PREV_DUP = 24;
    int DB_PREV_NODUP = 25;
    int DB_PRINTABLE = 0x0000020;
    int DB_PRIORITY_DEFAULT = 3;
    int DB_PRIORITY_HIGH = 4;
    int DB_PRIORITY_LOW = 2;
    int DB_PRIORITY_VERY_HIGH = 5;
    int DB_PRIORITY_VERY_LOW = 1;
    int DB_PRIVATE = 0x0800000;
    int DB_QUEUE = 4;
    int DB_RDONLY = 0x0000020;
    int DB_READ_COMMITTED = 0x04000000;
    int DB_READ_UNCOMMITTED = 0x08000000;
    int DB_RECNO = 3;
    int DB_RECNUM = 0x00080000;
    int DB_RECOVER = 0x0000040;
    int DB_RECOVER_FATAL = 0x1000000;
    int DB_REGION_INIT = 0x20000000;
    int DB_REGISTER = 0x2000000;
    int DB_RENUMBER = 0x00100000;
    int DB_REPMGR_ACKS_ALL = 1;
    int DB_REPMGR_ACKS_ALL_PEERS = 2;
    int DB_REPMGR_ACKS_NONE = 3;
    int DB_REPMGR_ACKS_ONE = 4;
    int DB_REPMGR_ACKS_ONE_PEER = 5;
    int DB_REPMGR_ACKS_QUORUM = 6;
    int DB_REPMGR_CONNECTED = 0x01;
    int DB_REPMGR_PEER = 0x01;
    int DB_REP_ACK_TIMEOUT = 1;
    int DB_REP_ANYWHERE = 0x0000001;
    int DB_REP_CHECKPOINT_DELAY = 2;
    int DB_REP_CLIENT = 1;
    int DB_REP_CONF_BULK = 0x0001;
    int DB_REP_CONF_DELAYCLIENT = 0x0002;
    int DB_REP_CONF_NOAUTOINIT = 0x0004;
    int DB_REP_CONF_NOWAIT = 0x0008;
    int DB_REP_CONNECTION_RETRY = 3;
    int DB_REP_DEFAULT_PRIORITY = 100;
    int DB_REP_ELECTION = 2;
    int DB_REP_ELECTION_RETRY = 4;
    int DB_REP_ELECTION_TIMEOUT = 5;
    int DB_REP_FULL_ELECTION_TIMEOUT = 6;
    int DB_REP_IGNORE = -30983;
    int DB_REP_ISPERM = -30982;
    int DB_REP_MASTER = 3;
    int DB_REP_NEWSITE = -30978;
    int DB_REP_NOBUFFER = 0x0000002;
    int DB_REP_NOTPERM = -30977;
    int DB_REP_PERMANENT = 0x0000004;
    int DB_REP_REREQUEST = 0x0000008;
    int DB_REVSPLITOFF = 0x00200000;
    int DB_RMW = 0x40000000;
    int DB_RPCCLIENT = 0x0000002;
    int DB_SALVAGE = 0x0000040;
    int DB_SEQ_DEC = 0x00000001;
    int DB_SEQ_INC = 0x00000002;
    int DB_SEQ_WRAP = 0x00000008;
    int DB_SET = 26;
    int DB_SET_LOCK_TIMEOUT = 1;
    int DB_SET_RANGE = 27;
    int DB_SET_RECNO = 28;
    int DB_SET_TXN_TIMEOUT = 3;
    int DB_SNAPSHOT = 0x00400000;
    int DB_STAT_CLEAR = 0x0000004;
    int DB_SYSTEM_MEM = 0x4000000;
    int DB_THREAD = 0x0000080;
    int DB_TIME_NOTGRANTED = 0x40000000;
    int DB_TRUNCATE = 0x0000100;
    int DB_TXN_ABORT = 0;
    int DB_TXN_APPLY = 1;
    int DB_TXN_BACKWARD_ROLL = 3;
    int DB_TXN_FORWARD_ROLL = 4;
    int DB_TXN_NOSYNC = 0x0000200;
    int DB_TXN_NOT_DURABLE = 0x0000800;
    int DB_TXN_NOWAIT = 0x0000400;
    int DB_TXN_PRINT = 7;
    int DB_TXN_SNAPSHOT = 0x10000000;
    int DB_TXN_SYNC = 0x0004000;
    int DB_TXN_WAIT = 0x0008000;
    int DB_TXN_WRITE_NOSYNC = 0x0001000;
    int DB_UNKNOWN = 5;
    int DB_UPGRADE = 0x0000001;
    int DB_USE_ENVIRON = 0x0004000;
    int DB_USE_ENVIRON_ROOT = 0x0008000;
    int DB_VERB_DEADLOCK = 0x0001;
    int DB_VERB_FILEOPS = 0x0002;
    int DB_VERB_FILEOPS_ALL = 0x0004;
    int DB_VERB_RECOVERY = 0x0008;
    int DB_VERB_REGISTER = 0x0010;
    int DB_VERB_REPLICATION = 0x0020;
    int DB_VERB_WAITSFOR = 0x0040;
    int DB_VERIFY = 0x0000002;
    int DB_VERSION_MAJOR = 4;
    int DB_VERSION_MINOR = 6;
    int DB_VERSION_PATCH = 19;
    int DB_WRITECURSOR = 30;
    int DB_XA_CREATE = 0x0000002;
    int DB_XIDDATASIZE = 128;
    int DB_YIELDCPU = 0x80000000;
}

// end of DbConstants.java
