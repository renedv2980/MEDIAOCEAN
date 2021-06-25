*          DATA SET CTGEN24X   AT LEVEL 002 AS OF 08/22/00                      
*                                                                               
*PHASE TA0B24A                                                                  
*                                                                               
         TITLE 'CTGEN24 - File Maintenance - Script Editor'                     
GEN23    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*GEN23**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC              VALREC here                                  
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
* Routine to validate key of record                                   *         
***********************************************************************         
*                                                                               
VALKEY   EQU   *                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CT7REC,R2                                                        
         MVC   APWORK,SPACES                                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         MVI   CT7KTYP,CT7KTYPQ      Record Code                                
         MVC   CT7KCODE,SPACES                                                  
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL,SCRNAMEH        Get Name of Script                         
         BNE   VALKEYX                                                          
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CT7KCODE(0),SCRNAME                                              
*                                                                               
         MVC   APRECKEY(L'CT7KEY),CT7KEY Save this key                          
         DROP  R2                                                               
*                                                                               
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS       Display only?                                
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       No - Read for Update then                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O Error exit here                          
         BNE   *+12                Record not found?                            
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        Test Record Deleted                          
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     Not Deleted, so no record exists             
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES Deleted record found                    
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK) OK exit point here                           
VALKEYX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A Script RECORD                                      
***********************************************************************         
*                                                                               
VALREC   L     R2,AIOAREA1         Record built here                            
         USING CT7KEY,R2                                                        
         MVC   CT7KEY(L'CT7KEY),APRECKEY                                        
         CLI   APACTN,ACTADD       Add?                                         
         BE    VR010               No Elements to remove on an Add              
         MVI   APELEM,CTSCRELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CT7REC      Delete Existing Script Elements              
*                                                                               
VR010    LA    R1,CT7KEYL          Define initial record length                 
         STCM  R1,3,CT7LEN         and shove it into record                     
         XC    APELEM,APELEM       Clear element area                           
         LA    R3,APELEM                                                        
         USING CTSCRD,R3           R3=A(Script Element)                         
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
*                                                                               
         MVI   CTSCREL,CTSCRELQ    Element Code                                 
         LA    R1,1                First time in - sequence # is 1              
         STC   R1,CTSCRSEQ                                                      
         STC   R1,APHALF+1         Save sequence #                              
*                                                                               
         LA    R4,SCRTXT1H         First input text line                        
         LA    RF,CTSCRLNQ         L'fixed stuff before data -                  
         STC   RF,CTSCRLEN         A(Data)                                      
*                                                                               
VR015    GOTO1 AFVAL,(R4)          Any I/P on this Line?                        
         BNE   VR025               No - Bump to next then                       
         LA    R3,APELEM                                                        
         USING CTSCRD,R3                                                        
         ZIC   R0,CTSCRLEN         Current length of element                    
         AR    R3,R0               First free data slot                         
         ZIC   RF,FVXLEN           L'input -1                                   
         LR    RE,RF               Input length -1 used for move                
         LA    RF,1(RF)            Real length of I/P                           
         LH    R1,=H'255'                                                       
         SR    R1,R0               Amount that may be moved in                  
         CR    R1,RF               Can it all be moved in?                      
         BH    VR20                Yes..                                        
*                                                                               
         SR    RF,R1               RF holds what must be moved after            
         STC   RF,APHALF           this element is saved                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
         LA    R3,APELEM                                                        
         LA    R1,255                                                           
         STC   R1,CTSCRLEN                                                      
         MVC   SCTABNAM,=CL7'CTFBIG '                                           
         GOTO1 AADDELS,CT7REC      Add this element                             
         BNE   VALRECX                                                          
*                                                                               
         XC    APELEM,APELEM       Move in remainder and set lengths            
         LA    R3,APELEM                                                        
         MVI   CTSCREL,CTSCRELQ                                                 
         ZIC   RE,FVILEN           L' what`s input                              
         ZIC   RF,APHALF           L' what`s left                               
         SR    RE,RF               L' what`s in already                         
         LA    RE,FVIFLD(RE)       A(What`s left to come in)                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTSCRDTA(0),0(RE)                                                
         LA    RF,4(RF)            3 for fixed 1 for mvc                        
         STC   RF,CTSCRLEN                                                      
         ZIC   R1,APHALF+1                                                      
         LA    R1,1(R1)                                                         
         STC   R1,APHALF+1         New sequence #                               
         STC   R1,CTSCRSEQ                                                      
         B     VR025                                                            
*                                                                               
VR20     EQU   *                   OK to move in all this line                  
         EX    RE,*+8              Move in this line of input                   
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
         AR    RF,R0               R0 holds current length                      
         LA    R3,APELEM                                                        
         STC   RF,CTSCRLEN         Save new length                              
         B     VR025               Bump to next line                            
*                                                                               
VR025    CLI   APBYTE,MAXTXTNO     Max # Lines on Screen                        
         BNL   VR030                                                            
         ZIC   RF,APBYTE           Bump Line Count to next line                 
         LA    RF,1(RF)            ..                                           
         STC   RF,APBYTE           and save                                     
         LH    RE,APHALF                                                        
         LA    R3,APELEM                                                        
         LA    R3,0(RE,R3)         Next Data in APELEM goes here                
         AH    R4,TXTLEN           Bump to next line on screen                  
         B     VR015               Do it again                                  
*                                                                               
VR030    MVC   SCTABNAM,=CL7'CTFBIG '                                           
         GOTO1 AADDELS,CT7REC      Last time in                                 
*                                                                               
         CLI   APACTN,ACTADD                                                    
         BE    *+12                                                             
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         B     *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                 Add the record                               
         BE    *+6                                                              
         DC    H'0'                Die if N.O.K.                                
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK) OK message                                   
*                                                                               
VALRECX  B     EXIT                That`s all folks...                          
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* Routine to display key of record                                    *         
***********************************************************************         
*                                                                               
DISKEY   LA    R2,APRECKEY                                                      
         USING CT7REC,R2           Script object code                           
         MVC   APWORKX,SPACES      Preset to spaces                             
         MVC   APWORK(L'CT7KCODE),CT7KCODE                                      
         GOTO1 DISPFLD,SCRNAMEH    Display the Name                             
         DROP  R2                                                               
DISKEYX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* ROUTINE TO DISPLAY Script RECORD                                              
***********************************************************************         
*                                                                               
DISREC   L     R2,AIOAREA1         A(This Record)                               
         USING CT7REC,R2                                                        
         TWAXC SCRTXT1H,PROT=Y     Clear Data Portion of Screen                 
         XC    APELEM,APELEM                                                    
         XC    APWORK,APWORK                                                    
*                                                                               
DREC200  XC    APELEM,APELEM                                                    
         MVI   APELEM,CTSCRELQ     Element Code                                 
         GOTO1 AGETELS,CT7REC      Get A(First matching element)                
         ICM   R3,15,APPARM        R3=A(First matching element)                 
         USING CTSCRD,R3                                                        
         BZ    DISREC2X            No matches                                   
         LA    R4,SCRTXT1H         First Data field on screen                   
         XC    APBYTE,APBYTE       # of Lines displayed so far                  
*                                                                               
DREC210  MVC   APWORKX,SPACES      Preset APWORK to spaces                      
         LA    R8,CTSCRDTA         A(Data in Element)                           
         ZIC   R1,CTSCRLEN         Get Element Length                           
         LA    RF,CTSCRLNQ         L'crap before data                           
         SR    R1,RF               Get Data Length                              
         STH   R1,APHALF           and Save it                                  
*                                                                               
DREC220  LH    R1,APHALF           Length of a line has been                    
         CH    R1,=H'63'           Hard - Coded for now                         
         BNH   DREC230                                                          
         LA    R1,62               Move in 1 Screen Line of Data                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),0(R8)     Move data into APWORK                        
         LA    R1,0(R4)                                                         
         GOTO1 DISPFLD             Display Data                                 
         MVC   APWORKX,SPACES      Set APWORK to spaces                         
         AH    R4,TXTLEN           Bump to next line on screen                  
         LH    R1,APHALF           Reduce undisplayed amount in elem.           
         SH    R1,=H'63'           ..                                           
         STH   R1,APHALF           and save it..                                
         LA    R8,63(R8)           Next data in record                          
         ZIC   R1,APBYTE           Increment line count                         
         LA    R1,1(R1)            ..                                           
         STC   R1,APBYTE           and save it..                                
         CLI   R1,MAXTXTNO         Full Screen yet?                             
         BL    DREC220             No, keep on going.                           
         B     DISREC2X                                                         
*                                                                               
DREC230  MVC   APWORKX,SPACES      Preset APWORK to Spaces again                
         LH    R1,APHALF           L'what`s left to go out                      
         BCTR  R1,0                                                             
         EX    R1,*+8              Move it in                                   
         B     *+10                                                             
         MVC   APWORK(0),0(R8)                                                  
         LA    R1,0(R4)            R4=A(Field Hdr)                              
         GOTO1 DISPFLD             Display data                                 
         CLI   APBYTE,MAXTXTNO     Full Screen Yet?                             
         BNL   DISREC2X                                                         
         AH    R4,TXTLEN           Bump to next line on screen                  
         ZIC   R1,APBYTE           Bump # of Lines by 1                         
         LA    R1,1(R1)            ..                                           
         STC   R1,APBYTE           and save it..                                
         BAS   RE,NEXTEL           Get next element R3=A(Current El)            
         BE    DREC210             Do it all again if another el. found         
         DROP  R3                                                               
*                                                                               
DISREC2X GOTO1 ADISACT,CT7REC                                                   
         B     EXIT                                                             
*                                                                               
***********************************************************************         
* Routine to Delete a Script Record                               Done          
***********************************************************************         
*                                                                               
DELREC   L     R2,AIOAREA1                                                      
         OI    CT7STAT,X'80'       Set Delete Flag on in Record                 
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                Die if N.O.K                                 
DELRECX  B     EXIT                That`s all folks...                          
*                                                                               
***********************************************************************         
* Routine to restore a Deleted Script Record                      Done          
***********************************************************************         
*                                                                               
RESREC   L     R2,AIOAREA1                                                      
         NI    CT7STAT,X'FF'-X'80' Turn off Delete Flag in Record               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                Die if N.O.K                                 
RESRECX  B     EXIT                Easy life...                                 
*                                                                               
***********************************************************************         
* Routine to Validate Select Parameters                               *         
***********************************************************************         
*                                                                               
VALSEL   LA    R2,APRECKEY                                                      
         TM    APINDS,APILFLST     First time in?                               
         BZ    *+8                                                              
         OI    APINDS,APILFLST     Yes                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
         USING CT7REC,R2                                                        
         MVI   CT7KTYP,CT7KTYPQ                                                 
         MVC   CT7KCODE,SPACES                                                  
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL,LSTNAMEH                                                   
         BNE   VALSELY                                                          
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CT7KCODE(0),LSTNAME                                              
*                                                                               
         MVC   IOKEY(L'CT7KEY),CT7KEY Save this key                             
         DROP  R2                                                               
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R0,LSTACT1H         A(Line 1) of List screen                     
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H                                                      
         SR    R1,R0               Length of List line                          
         STH   R1,APPARM+6                                                      
VALSELX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* Get next List/Select Record                                         *         
***********************************************************************         
*                                                                               
GETSEL   LA    R2,IOKEY                                                         
         USING CT7REC,R2                                                        
         MVC   CT7KEY,APRECKEY     Get saved Key                                
         TM    APINDS,APILFLST     Test First Time In                           
         BZ    GETSEL2                                                          
         B     GETSEL6             Read High                                    
GETSEL2  TM    APINDS,APILRERD     Test Sequence Broken by User I/O             
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,APILNSEQ     Test Read or ReadHI                          
         BO    GETSEL8                                                          
GETSEL6  LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN             EOF Exit                                     
         L     R2,AIOAREA1                                                      
*                                                                               
GETSEL20 CLI   CT7KTYP,CT7KTYPQ    Check it is still a Script Record            
         BNE   GETSELN             EOF Exit                                     
         CLI   CT7SEQNO,0          First of a series only                       
         BNE   GETSEL8                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK) OK Exit point                                
         MVC   APRECKEY(L'CT7KEY),CT7KEY Save Key                               
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      Set EOF                                      
*                                                                               
GETSELX  B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* Display List/Select Line                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         USING CT7REC,R2                                                        
         ICM   R4,15,APPARM                                                     
         USING LISTLIND,R4                                                      
*                                                                               
         MVC   LENAMEO,CT7KCODE    Shove out name for now                       
         DROP  R2                                                               
*                                                                               
DISSELX  B     EXIT                                                             
         DROP  R4                                                               
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   B     EXIT                                                             
VALREQX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* ROUTINE TO GENERATE MESSAGE REPORT                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
*                                                                               
**********************************************************************          
* General Field Transmit if Changed                                  *          
* R1=A(TWA Header)                                                   *          
* APWORK must contain the new text                                   *          
**********************************************************************          
*                                                                               
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     Knock off Header Extension                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         Compare Field contents                       
         BER   RE                  Equal don`t bother to move in data           
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         Move in new data                             
         BR    RE                                                               
*                                                                               
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
*                                                                               
***********************************************************************         
* Find next element of same code within Record                        *         
* Entry      R3 = A(Current Element)                                  *         
*        APELEM = Element Code to find                                *         
* Exit    CC EQ - Found - R3=A(New Element)                           *         
*         CC NE - Not Found                                           *         
***********************************************************************         
*                                                                               
NEXTEL   ZIC   RF,1(R3)            L'Element                                    
         AR    R3,RF               A(Next element)                              
         ICM   RF,1,1(R3)          L'Element                                    
         BNZ   *+8                                                              
         LTR   RB,RB               Force CC Non Zero                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM      Same as last?                                
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
         LTORG                                                                  
*                                                                               
SPACES   DC    CL132' '                                                         
STARS    DC    80C'*'                                                           
*                                                                               
TXTLEN   DC    Y(SCRTXT2-SCRTXT1)                      L LINE                   
MAXTXTNO EQU   (SCRLAST-SCRTXT1)/(SCRTXT2-SCRTXT1)     # lines                  
*                                                                               
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
*                                                                               
LOCALX   EQU   *                                                                
*                                                                               
       ++INCLUDE FAUTL                                                          
*                                                                               
* CTGENWRK                                                                      
* FAGETTXTD                                                                     
         PRINT  OFF                                                             
       ++INCLUDE CTGENWRK                                                       
       ++INCLUDE FAGETTXTD                                                      
         PRINT  ON                                                              
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENC6D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENCDD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENABD                                                       
*                                                                               
*                                                                               
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
*                                                                               
SELKEY   DS    0XL25                                                            
SELSYS   DS    XL1                 Script Code                                  
SELSPR   DS    XL14                Spare for now                                
SELNAME  DS    XL8                 Script Name                                  
SELSEQN  DS    XL2                 Sequence Number                              
*                                                                               
LISTLIND DSECT                                                                  
         DS    CL8                                                              
         DS    CL3                                                              
         DS    CL8                                                              
LENAME   DS    CL10                                                             
         ORG   LENAME                                                           
LENAMEO  DS    CL8                                                              
         DS    CL2                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTGEN24X  08/22/00'                                      
         END                                                                    
