package Tk::English;

require Exporter;

use vars qw($VERSION);
$VERSION = '4.006'; # $Id: //depot/Tkutf8/Tk/English.pm#6 $

use base  qw(Exporter);

# This file is generated automatically by pTk/makeenglish from Tk distribution.


@EXPORT = qw(
    &ABOVE &ACTIVATE &ACTIVE &ADD &ADDTAG &ADJUST &AFTER &ALL &ANCHOR &APPEND
    &APPLY &ARROW1 &ARROW2 &ASCII &ASPECT &AUTO &BASELINE &BBOX &BEFORE &BELOW
    &BEVEL &BIND &BITMAP &BLANK &BOTH &BOTTOM &BUTT &CANVASX &CANVASY &CAPTURE
    &CASCADE &CENTER &CGET &CHAR &CHARS &CHECKBUTTON &CHILDREN &CLEAR &CLIENT
    &CLONE &CLOSEST &COLOR &COLORMAPWINDOWS &COLUMN &COLUMNCONFIGURE &COMMAND
    &COMPARE &CONFIGURE &COORDS &COPY &CREATE &CURRENT &CURSELECTION &DATA
    &DCHARS &DEACTIVATE &DEBUG &DECORATIONS &DECREASING &DEFAULT &DEICONIFY
    &DELETE &DELTA &DESELECT &DLINEINFO &DOWN &DRAGSITE &DRAGTO &DROPSITE &DTAG
    &DUMP &ENCLOSED &END &ENTRY &ENTRYCGET &ENTRYCONFIGURE &EVAL &EXISTS &EXPAND
    &FILL &FILLX &FILLY &FIND &FIRST &FLASH &FLAT &FOCUS &FOCUSMODEL &FOCUSNEXT
    &FOCUSPREV &FORGET &FRACTION &FRAME &FROM &GENERATE &GEOMETRY &GEOMETRYINFO
    &GET &GETTAGS &GRAVITY &GRAY &GRID &GROOVE &GROUP &HANDLE &HEAD &HEIGHT
    &HIDDEN &HIDE &HORIZONTAL &ICONBITMAP &ICONIFY &ICONMASK &ICONNAME
    &ICONPOSITION &ICONWINDOW &ICURSOR &IDENTIFY &IDLETASKS &IGNORE &IMAGE
    &INCLUDES &INCREASING &INDEX &INFO &INSERT &INSIDE &INTEGER &INTERACTIVE
    &INVOKE &ISMWMRUNNING &ITEM &ITEMCGET &ITEMCONFIGURE &LAST &LEFT &LINE
    &LINECONFIGURE &LINEEND &LINES &LINESTART &LIST &LOCATION &LOWER &MARK &MAX
    &MAXSIZE &MENUBAR &MINSIZE &MITER &MONOCHROME &MOVE &MOVETO &NAMES &NEAREST
    &NEW &NEXT &NEXTRANGE &NONE &NORMAL &NOW &OFFSPRINGS &OUTSIDE &OVERLAPPING
    &OVERRIDEREDIRECT &OWN &PADX &PADY &PAGECGET &PAGECONFIGURE &PAGES &PARENT
    &PASSIVE &POSITION &POSITIONFROM &POST &POSTCASCADE &POSTSCRIPT &PRESENT
    &PREV &PREVIOUS &PREVRANGE &PROGRAM &PROJECTING &PROPAGATE &PROTOCOL &PUT
    &RADIOBUTTON &RAISE &RAISED &RANGE &RANGES &READ &READFILE &REAL &RECORD
    &REDITHER &REFCOUNT &RELEASE &REMOVE &RESIZABLE &RIDGE &RIGHT &ROOT &ROUND
    &ROW &ROWCONFIGURE &ROWS &SAVEUNDER &SCALE &SCAN &SCROLL &SEARCH &SEE
    &SELECT &SELECTION &SEPARATOR &SET &SHOW &SIBLINGS &SIZE &SIZEFROM &SLAVES
    &SLIDER &SOLID &SPACE &STATE &STATUS &SUNKEN &TAG &TAIL &TEAROFF &TEXT
    &TITLE &TO &TOGGLE &TOP &TRACING &TRANSIENT &TRANSIENTFOR &TYPE &TYPES
    &UNITS &UNPACK &UNPOST &UNSET &UP &USER &VARIABLE &VERTICAL &VISIBILITY
    &WIDTH &WINDOW &WITHDRAW &WITHTAG &WORDEND &WORDSTART &WRITE &XVIEW
    &YPOSITION &YVIEW
);
sub ABOVE () { 'above' }
sub ACTIVATE () { 'activate' }
sub ACTIVE () { 'active' }
sub ADD () { 'add' }
sub ADDTAG () { 'addtag' }
sub ADJUST () { 'adjust' }
sub AFTER () { 'after' }
sub ALL () { 'all' }
sub ANCHOR () { 'anchor' }
sub APPEND () { 'append' }
sub APPLY () { 'apply' }
sub ARROW1 () { 'arrow1' }
sub ARROW2 () { 'arrow2' }
sub ASCII () { 'ascii' }
sub ASPECT () { 'aspect' }
sub AUTO () { 'auto' }
sub BASELINE () { 'baseline' }
sub BBOX () { 'bbox' }
sub BEFORE () { 'before' }
sub BELOW () { 'below' }
sub BEVEL () { 'bevel' }
sub BIND () { 'bind' }
sub BITMAP () { 'bitmap' }
sub BLANK () { 'blank' }
sub BOTH () { 'both' }
sub BOTTOM () { 'bottom' }
sub BUTT () { 'butt' }
sub CANVASX () { 'canvasx' }
sub CANVASY () { 'canvasy' }
sub CAPTURE () { 'capture' }
sub CASCADE () { 'cascade' }
sub CENTER () { 'center' }
sub CGET () { 'cget' }
sub CHAR () { 'char' }
sub CHARS () { 'chars' }
sub CHECKBUTTON () { 'checkbutton' }
sub CHILDREN () { 'children' }
sub CLEAR () { 'clear' }
sub CLIENT () { 'client' }
sub CLONE () { 'clone' }
sub CLOSEST () { 'closest' }
sub COLOR () { 'color' }
sub COLORMAPWINDOWS () { 'colormapwindows' }
sub COLUMN () { 'column' }
sub COLUMNCONFIGURE () { 'columnconfigure' }
sub COMMAND () { 'command' }
sub COMPARE () { 'compare' }
sub CONFIGURE () { 'configure' }
sub COORDS () { 'coords' }
sub COPY () { 'copy' }
sub CREATE () { 'create' }
sub CURRENT () { 'current' }
sub CURSELECTION () { 'curselection' }
sub DATA () { 'data' }
sub DCHARS () { 'dchars' }
sub DEACTIVATE () { 'deactivate' }
sub DEBUG () { 'debug' }
sub DECORATIONS () { 'decorations' }
sub DECREASING () { 'decreasing' }
sub DEFAULT () { 'default' }
sub DEICONIFY () { 'deiconify' }
sub DELETE () { 'delete' }
sub DELTA () { 'delta' }
sub DESELECT () { 'deselect' }
sub DLINEINFO () { 'dlineinfo' }
sub DOWN () { 'down' }
sub DRAGSITE () { 'dragsite' }
sub DRAGTO () { 'dragto' }
sub DROPSITE () { 'dropsite' }
sub DTAG () { 'dtag' }
sub DUMP () { 'dump' }
sub ENCLOSED () { 'enclosed' }
sub END () { 'end' }
sub ENTRY () { 'entry' }
sub ENTRYCGET () { 'entrycget' }
sub ENTRYCONFIGURE () { 'entryconfigure' }
sub EVAL () { 'eval' }
sub EXISTS () { 'exists' }
sub EXPAND () { 'expand' }
sub FILL () { 'fill' }
sub FILLX () { 'fillx' }
sub FILLY () { 'filly' }
sub FIND () { 'find' }
sub FIRST () { 'first' }
sub FLASH () { 'flash' }
sub FLAT () { 'flat' }
sub FOCUS () { 'focus' }
sub FOCUSMODEL () { 'focusmodel' }
sub FOCUSNEXT () { 'focusnext' }
sub FOCUSPREV () { 'focusprev' }
sub FORGET () { 'forget' }
sub FRACTION () { 'fraction' }
sub FRAME () { 'frame' }
sub FROM () { 'from' }
sub GENERATE () { 'generate' }
sub GEOMETRY () { 'geometry' }
sub GEOMETRYINFO () { 'geometryinfo' }
sub GET () { 'get' }
sub GETTAGS () { 'gettags' }
sub GRAVITY () { 'gravity' }
sub GRAY () { 'gray' }
sub GRID () { 'grid' }
sub GROOVE () { 'groove' }
sub GROUP () { 'group' }
sub HANDLE () { 'handle' }
sub HEAD () { 'head' }
sub HEIGHT () { 'height' }
sub HIDDEN () { 'hidden' }
sub HIDE () { 'hide' }
sub HORIZONTAL () { 'horizontal' }
sub ICONBITMAP () { 'iconbitmap' }
sub ICONIFY () { 'iconify' }
sub ICONMASK () { 'iconmask' }
sub ICONNAME () { 'iconname' }
sub ICONPOSITION () { 'iconposition' }
sub ICONWINDOW () { 'iconwindow' }
sub ICURSOR () { 'icursor' }
sub IDENTIFY () { 'identify' }
sub IDLETASKS () { 'idletasks' }
sub IGNORE () { 'ignore' }
sub IMAGE () { 'image' }
sub INCLUDES () { 'includes' }
sub INCREASING () { 'increasing' }
sub INDEX () { 'index' }
sub INFO () { 'info' }
sub INSERT () { 'insert' }
sub INSIDE () { 'inside' }
sub INTEGER () { 'integer' }
sub INTERACTIVE () { 'interactive' }
sub INVOKE () { 'invoke' }
sub ISMWMRUNNING () { 'ismwmrunning' }
sub ITEM () { 'item' }
sub ITEMCGET () { 'itemcget' }
sub ITEMCONFIGURE () { 'itemconfigure' }
sub LAST () { 'last' }
sub LEFT () { 'left' }
sub LINE () { 'line' }
sub LINECONFIGURE () { 'lineconfigure' }
sub LINEEND () { 'lineend' }
sub LINES () { 'lines' }
sub LINESTART () { 'linestart' }
sub LIST () { 'list' }
sub LOCATION () { 'location' }
sub LOWER () { 'lower' }
sub MARK () { 'mark' }
sub MAX () { 'max' }
sub MAXSIZE () { 'maxsize' }
sub MENUBAR () { 'menubar' }
sub MINSIZE () { 'minsize' }
sub MITER () { 'miter' }
sub MONOCHROME () { 'monochrome' }
sub MOVE () { 'move' }
sub MOVETO () { 'moveto' }
sub NAMES () { 'names' }
sub NEAREST () { 'nearest' }
sub NEW () { 'new' }
sub NEXT () { 'next' }
sub NEXTRANGE () { 'nextrange' }
sub NONE () { 'none' }
sub NORMAL () { 'normal' }
sub NOW () { 'now' }
sub OFFSPRINGS () { 'offsprings' }
sub OUTSIDE () { 'outside' }
sub OVERLAPPING () { 'overlapping' }
sub OVERRIDEREDIRECT () { 'overrideredirect' }
sub OWN () { 'own' }
sub PADX () { 'padx' }
sub PADY () { 'pady' }
sub PAGECGET () { 'pagecget' }
sub PAGECONFIGURE () { 'pageconfigure' }
sub PAGES () { 'pages' }
sub PARENT () { 'parent' }
sub PASSIVE () { 'passive' }
sub POSITION () { 'position' }
sub POSITIONFROM () { 'positionfrom' }
sub POST () { 'post' }
sub POSTCASCADE () { 'postcascade' }
sub POSTSCRIPT () { 'postscript' }
sub PRESENT () { 'present' }
sub PREV () { 'prev' }
sub PREVIOUS () { 'previous' }
sub PREVRANGE () { 'prevrange' }
sub PROGRAM () { 'program' }
sub PROJECTING () { 'projecting' }
sub PROPAGATE () { 'propagate' }
sub PROTOCOL () { 'protocol' }
sub PUT () { 'put' }
sub RADIOBUTTON () { 'radiobutton' }
sub RAISE () { 'raise' }
sub RAISED () { 'raised' }
sub RANGE () { 'range' }
sub RANGES () { 'ranges' }
sub READ () { 'read' }
sub READFILE () { 'readfile' }
sub REAL () { 'real' }
sub RECORD () { 'record' }
sub REDITHER () { 'redither' }
sub REFCOUNT () { 'refcount' }
sub RELEASE () { 'release' }
sub REMOVE () { 'remove' }
sub RESIZABLE () { 'resizable' }
sub RIDGE () { 'ridge' }
sub RIGHT () { 'right' }
sub ROOT () { 'root' }
sub ROUND () { 'round' }
sub ROW () { 'row' }
sub ROWCONFIGURE () { 'rowconfigure' }
sub ROWS () { 'rows' }
sub SCALE () { 'scale' }
sub SCAN () { 'scan' }
sub SCROLL () { 'scroll' }
sub SEARCH () { 'search' }
sub SEE () { 'see' }
sub SELECT () { 'select' }
sub SELECTION () { 'selection' }
sub SEPARATOR () { 'separator' }
sub SET () { 'set' }
sub SHOW () { 'show' }
sub SIBLINGS () { 'siblings' }
sub SIZE () { 'size' }
sub SIZEFROM () { 'sizefrom' }
sub SLAVES () { 'slaves' }
sub SLIDER () { 'slider' }
sub SOLID () { 'solid' }
sub SPACE () { 'space' }
sub STATE () { 'state' }
sub STATUS () { 'status' }
sub SUNKEN () { 'sunken' }
sub TAG () { 'tag' }
sub TAIL () { 'tail' }
sub TEAROFF () { 'tearoff' }
sub TEXT () { 'text' }
sub TITLE () { 'title' }
sub TO () { 'to' }
sub TOGGLE () { 'toggle' }
sub TOP () { 'top' }
sub TRACING () { 'tracing' }
sub TRANSIENT () { 'transient' }
sub TRANSIENTFOR () { 'transientfor' }
sub TYPE () { 'type' }
sub TYPES () { 'types' }
sub UNITS () { 'units' }
sub UNPACK () { 'unpack' }
sub UNPOST () { 'unpost' }
sub UNSET () { 'unset' }
sub UP () { 'up' }
sub USER () { 'user' }
sub VARIABLE () { 'variable' }
sub VERTICAL () { 'vertical' }
sub VISIBILITY () { 'visibility' }
sub WIDTH () { 'width' }
sub WINDOW () { 'window' }
sub WITHDRAW () { 'withdraw' }
sub WITHTAG () { 'withtag' }
sub WORDEND () { 'wordend' }
sub WORDSTART () { 'wordstart' }
sub WRITE () { 'write' }
sub XVIEW () { 'xview' }
sub YPOSITION () { 'yposition' }
sub YVIEW () { 'yview' }

1;
