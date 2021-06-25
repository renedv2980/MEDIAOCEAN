*          DATA SET SRSCP00    AT LEVEL 003 AS OF 02/06/97                      
*PHASE T11D00A                                                                  
*INCLUDE SCRUMPY                                                                
*INCLUDE HEXOUT                                                                 
         TITLE '$SCRIPT - Process a Script with Worker files'                   
         PRINT NOGEN                                                            
*                                                                               
SCRIPT   CSECT                                                                  
         NMODL SRWORKX-SRWORKD,**$SCP**,RA,RR=R3,CLEAR=YES                      
         USING SRWORKD,RC                                                       
         ST    R3,RELO                                                          
         USING SRPARMD,R1                                                       
         MVC   ATIA,SRPARM2        Save A(TIA) - First I/O Buffer               
         L     R9,SRPARM1          R9=A(SYSFAC)                                 
         USING SYSFACD,R9                                                       
         L     R8,SRPARM3          R8=A(UTL ENTRY)                              
         USING UTLD,R8                                                          
         L     R7,SRPARM4          R7=A(COMFACS)                                
         USING COMFACSD,R7                                                      
         L     R6,SRPARM6          R6=A(TWA)                                    
         USING SRSCPFFD,R6                                                      
         DROP  R1                                                               
*                                                                               
         LH    R1,DIOIN           Get addressibility to all the I/O             
         AR    R1,RC              areas used for worker files                   
         ST    R1,AIOIN           ..                                            
         LH    R1,DIOOUT          ..                                            
         AR    R1,RC              ..                                            
         ST    R1,AIOOUT          ..                                            
         LH    R1,DERRBUFF        ..                                            
         AR    R1,RC              ..                                            
         ST    R1,AERRBUFF        ..                                            
         LH    R1,DBUFFER         ..                                            
         AR    R1,RC              ..                                            
         ST    R1,ABUFFER         ..                                            
*                                                                               
         TM    TSVCREQ,X'01'                                                    
         BZ    *+8                                                              
         OI    TSVCREQ,X'02'                                                    
*                                                                               
         CLC   SRVSRV+7(5),=C',BALL' Backout instructions on error              
         BNE   P1VAL                                                            
         LA    R1,SCRIPTPL                                                      
         USING SCPLD,R1                                                         
         OI    SCPLINDS,SCPLBALL   Unwind transactions on abend flag            
         DROP  R1                                                               
*                                                                               
***********************************************************************         
* Validate Script Name parameter                                      *         
***********************************************************************         
*                                                                               
P1VAL    LA    R2,SRVP1H           Either Script name or from screen            
         MVI   WHY,0                                                            
         CLI   SRVP1H+5,0          Input this field?                            
         BNE   P1V010                                                           
         MVI   WHY,3               flag no input                                
         B     P4VAL               allowed for worker file input                
P1V010   CLI   SRVP1,C'*'          * MEANS DISPLAY A SHORT SCRIPT               
         BE    P1V012                                                           
         CLC   SRVP1(4),=C'TEXT'   Means Script to be typed in                  
         BE    S1VAL                                                            
         CLC   SRVP1(5),=C'DIRECT' Means Script to be typed in                  
         BE    S1VAL                                                            
         MVC   SCRID,SRVP1         Name of Script will be passed to             
         OC    SCRID,SPACES        FASCRIPT directly- name is space             
         B     P1V050              filled                                       
*                                                                               
P1V012   CLI   SRVP1H+5,1          *SCRIPTID MEANS READ AND DISPLAY             
         BE    S1VAL                                                            
         CLI   SRVP1+1,C'*'        **SCRIPTID is left over from last            
         BNE   P1V014              TIME                                         
         MVC   SCRID,SRVP1+2       SAVE SCRIPT NAME FOR DISPLAY AGAIN           
         OC    SCRID,SPACES        Space fill it                                
         B     S1VAL               TIME                                         
P1V014   MVC   SCRID,SRVP1+1       SAVE SCRIPT NAME FOR DISPLAY AGAIN           
         OC    SCRID,SPACES        Space fill it                                
         XC    SRVP1,SRVP1                                                      
         MVC   SRVP1(2),=C'**'     Change to **SCRIPTID so next time it         
         MVC   SRVP1+2(8),SCRID    go and validate - redisplay it               
*                                                                               
P1V020   LA    R1,KEY              Read CTFILE to get a Script                  
         USING CT7REC,R1                                                        
         MVI   CT7KTYP,CT7KTYPQ    Build Script key                             
         MVC   CT7KCODE,SCRID                                                   
         GOTO1 CDATAMGR,DMCB,DMREAD,CTFILE,KEY,RECORD                           
         CLI   8(R1),0                                                          
         BE    *+12                OK?                                          
         MVI   ERRFLAG,2           No - Invalid script name                     
         B     ERREX                                                            
         LA    R1,RECORD                                                        
         LA    RF,CT7DATA          Point to the first element                   
         XR    R1,R1                                                            
         DROP  R1                                                               
*                                                                               
P1V030   CLI   0(RF),0             E.O.R.                                       
         BNE   *+12                                                             
         MVI   ERRFLAG,2           Invalid record                               
         B     ERREX                                                            
         CLI   0(RF),CTSCRELQ      Script Element?                              
         BE    P1V040              Yes                                          
         IC    R1,1(RF)            No - Bump                                    
         LA    RF,0(RF,R1)         ..                                           
         B     P1V030              and try the next                             
*                                                                               
P1V040   MVC   TEXT(252),3(RF)     Extract the script data from this            
         MVI   WHY,1               element, display and exit                    
         CLI   SRVP1,C'*'                                                       
         BE    DISPLAY                                                          
         MVI   WHY,2               Set display and return                       
         B     DISPLAY                                                          
*                                                                               
P1V050   LA    R2,SRVP1H           PASS SCRIPT NAME TO FASCRIPT                 
         CLI   5(R2),9             Script name only entered (<8 chars)          
         BL    *+12                                                             
         MVI   ERRFLAG,1           Invalid Script Name                          
         B     ERREX                                                            
         LA    R1,KEY              Read CTFILE to verify Script exists          
         USING CT7REC,R1                                                        
         MVI   CT7KTYP,CT7KTYPQ    Build Script Key                             
         MVC   CT7KCODE,SCRID      ..                                           
         DROP  R1                                                               
         GOTO1 CDATAMGR,DMCB,DMREAD,CTFILE,KEY,RECORD                           
         CLI   8(R1),0                                                          
         BE    P4VAL               OK - now go and see about I/O                
         MVI   ERRFLAG,2                                                        
         B     ERREX                                                            
*                                                                               
***********************************************************************         
* Validate a script from the screen                                   *         
***********************************************************************         
*                                                                               
S1VAL    LA    R2,SRVT1H           VALIDATE SCRIPT TEXT ON SCREEN               
         LA    R0,4                Number of screen lines for Script            
         LA    RE,TEXT             A(Script Text Buffer)                        
*                                                                               
S1V010   SR    R1,R1                                                            
         ICM   R1,1,5(R2)          Test for data on this line                   
         BZ    S1V020              None                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)       Move it into buffer                          
         LA    RE,1(R1,RE)         Bump to next free in buffer                  
*                                                                               
S1V020   ICM   R1,1,0(R2)          Next line on screen                          
         LA    R2,0(R1,R2)                                                      
         BCT   R0,S1V010           Any input lines left?                        
*                                                                               
         LA    R2,SRVT1H                                                        
         LA    R0,TEXT             Get length of Script text input              
         SR    RE,R0               ..                                           
         BNZ   *+12                                                             
         MVI   ERRFLAG,1           Nothing input                                
         B     ERREX                                                            
         OI    IOFLAG,SCR_SCRN     Set parameter for screen-script              
         B     P4VAL               Validate I/O parameters                      
*                                                                               
***********************************************************************         
* Display script data onto the screen                                 *         
***********************************************************************         
*                                                                               
DISPLAY  LA    RF,TEXT             Get the length of data to O/P                
         LA    R0,280              Maximum length                               
DISP010  OC    0(2,RF),0(RF)       Any characters?                              
         BZ    DISP020             No                                           
         CLC   0(2,RF),=C'**'      ** marks End of Script                       
         BNE   *+12                                                             
         LA    RF,2(RF)                                                         
         B     DISP020                                                          
         LA    RF,1(RF)            Bump ..                                      
         BCT   R0,DISP010          and try the next                             
DISP020  LA    RE,TEXT             Point to start of script                     
         SR    RF,RE               Get length of data                           
         LA    R2,SRVT1H           Point to first screen line                   
         LA    R0,4                Number of screen lines available             
DISP030  SR    R1,R1                                                            
         ICM   R1,1,0(R2)          Get the length of a display line             
         SH    R1,=H'9'            Header & Ex                                  
         BNP   DISP050                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       Clear this display line                      
         LA    R1,1(R1)                                                         
         LTR   RF,RF               Test if anything leftto display              
         BNP   DISP040             No                                           
         CR    R1,RF               More than will fit on this line?             
         BNH   *+6                                                              
         LR    R1,RF               R1=Length to display for this line           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(RE)       Move out the data to display                 
         LA    R1,1(R1)                                                         
         AR    RE,R1               Bump to next Script data to display          
         SR    RF,R1               Decrement undisplayed length                 
*                                                                               
DISP040  SR    R1,R1                                                            
         ICM   R1,1,0(R2)          Next free line on screen                     
         LA    R2,0(R1,R2)         ..                                           
         BCT   R0,DISP030          Any lines left?                              
*                                                                               
DISP050  LA    R2,SRVI1H                                                        
         CLI   WHY,1               Test for display and exit                    
         BNE   *+12                                                             
         MVI   ERRFLAG,99          Not really an error - but it works           
         B     ERREX                                                            
         B     EXIT                                                             
*                                                                               
***********************************************************************         
* Validate Input type parameter                                       *         
***********************************************************************         
*                                                                               
P2VAL    LA    R2,SRVP2H           Either worker file or from screen            
         CLI   5(R2),0             Nothing input - default to screen            
         BE    I1VAL                                                            
         CLC   SRVP2(4),=C'TEST'   Test for OK                                  
         BNE   *+12                                                             
         OI    IOFLAG,IP_TEST                                                   
         B     I1VAL                                                            
         CLC   SRVP2(4),=C'TEXT'   Input data from screen                       
         BE    I1VAL                                                            
         CLC   SRVP2(6),=C'DIRECT' INPUT DATA FROM SCREEN                       
         BE    I1VAL                                                            
         CLC   SRVP2(6),=C'SCREEN' INPUT FILE FROM SCREEN                       
         BNE   *+12                                                             
         OI    IOFLAG,IP_FILE                                                   
         B     I1VAL                                                            
*                                                                               
         XC    SRVT1,SRVT1                                                      
         XC    SRVT2,SRVT2                                                      
         XC    SRVT3,SRVT3                                                      
         XC    SRVT4,SRVT4                                                      
         XC    SRVI1,SRVI1                                                      
         XC    SRVI2,SRVI2                                                      
         XC    SRVI3,SRVI3                                                      
         XC    SRVI4,SRVI4                                                      
         XC    SRVI5,SRVI5                                                      
         XC    SRVI6,SRVI6                                                      
         LA    R2,WKEYIN           INPUT FROM WORKER FILE                       
         USING UKRECD,R2           ~~~~~~~~~~~~~~~~~~~~~~                       
         XC    WKEYIN,WKEYIN                                                    
         OC    TUSER,TUSER         Get Terminal                                 
         BNZ   P2V010                                                           
         TM    TSTAT1,TSTATDDS     Non-DDS Terminal must log on                 
         BNZ   *+16                                                             
         MVI   ERRFLAG,4                                                        
         LA    R2,SRVP2H                                                        
         B     ERREX                                                            
*&&UK*&& MVC   UKUSRID,=X'0026'    IF NOT CONNECTED USE DDS1                    
*&&US*&& MVC   UKUSRID,=X'002B'    IF NOT CONNECTED USE TCH1                    
         B     *+10                                                             
P2V010   MVC   UKUSRID,TUSER       From UTL                                     
         OC    IDNUM,IDNUM                                                      
         BZ    *+10                                                             
         MVC   UKUSRID,IDNUM       DIFFERENT USERID FOR DDS POSSIBLE            
         MVC   COMPNUM,UKUSRID                                                  
*                                                                               
         GOTO1 CSCANNER,DMCB,SRVP2H,SCANBLK                                     
         ZIC   R0,4(R1)                                                         
         LA    R3,SCANBLK                                                       
         USING SCANBLKD,R3         Code is the same as P3VAL below              
         CLI   SC1STLEN,1          ..                                           
         BNE   P2V012              ..                                           
         CLI   SC1STFLD,C'S'       ..                                           
         BNE   P2V012              ..                                           
*                                                                               
P2V011   TM    SC2NDVAL,SCNUMQ     ..                                           
         BO    *+16                ..                                           
         MVI   ERRFLAG,2           ..                                           
         LA    R2,SRVP2H                                                        
         B     ERREX               ..                                           
         L     R0,SC2NDNUM         ..                                           
         STCM  R0,3,UKFILENO       ..                                           
         MVI   UKFLAG,UKFLDAT      ..                                           
         B     P2V020                                                           
*                                                                               
P2V012   MVI   ERRFLAG,2                                                        
         LA    R2,SRVP2H                                                        
         B     ERREX                                                            
*                                                                               
P2V020   GOTO1 CDATAMGR,DMCB,GFILE,WRKFIL,WKEYIN,AIOIN,ATIA                     
         MVC   WRKFIN,UKUSRINF     Get the Worker File for this ID              
*                                                                               
         LA    R2,WKEYIN                                                        
         USING UKRECD,R2                                                        
         XC    WKEYIN,WKEYIN                                                    
         MVC   UKUSRID,COMPNUM                                                  
         MVI   UKFLAG,UKFLDAT                                                   
         STCM  R0,3,UKFILENO                                                    
         MVC   WKSVUSR,UKUSRID                                                  
         MVC   WKSVFNO,UKFILENO                                                 
         GOTO1 CDATAMGR,DMCB,(X'08',INDEX),=C'WRKFILE',WKEYIN,AIOIN,   *        
               ATIA                                                             
*        GOTO1 CDATAMGR,DMCB,(X'08',INDEX),WRKFIN,WKEYIN,AIOIN,ATIA             
         CLI   8(R1),0             Read OK?                                     
         BNE   INPERR              No                                           
         L     R1,AIOIN                                                         
         CLC   WKSVUSR,UKUSRID                                                  
         BNE   INPERR1             No                                           
         CLC   WKSVFNO,UKFILENO                                                 
         BNE   INPERR1             No                                           
*                                                                               
         TM    IOFLAG,WKR_SAME     Same file for Input and Output               
         BO    P2V025              Yes                                          
         GOTO1 CDATAMGR,DMCB,(X'08',READ),WRKFIN,WKEYIN,AIOIN,ATIA              
         CLI   8(R1),0             Read of file OK?                             
         BE    P2V030                                                           
         DC    H'0'                Must find at least 1 Record                  
*                                                                               
P2V025   GOTO1 CDATAMGR,DMCB,(X'88',READ),WRKFIN,WKEYIN,AIOIN,ATIA              
         CLI   8(R1),0             Read for update if files are same            
         BE    *+6                                                              
         DC    H'0'                Must find at least 1 Record                  
*                                                                               
P2V030   EQU   *                   EXECUTE SCRIPT WORKER FILE                   
*                                                                               
         GOTO1 =V(SCRUMPY),DMCB,(R7),ATIA,0,WKEYIN,0,RR=RELO                    
*                                                                               
         MVC   SCPERROR,16(R1)                                                  
         MVC   SCPOFF,18(R1)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,16(R1)         SCRIPT ERROR CODE AT PARM5                   
         BZ    P2V220                                                           
         CLM   RF,3,=AL2(9000)                                                  
         BE    P2V100                                                           
         CLM   RF,3,=AL2(0001)                                                  
         BE    P2V110                                                           
         CLM   RF,3,=AL2(1300)                                                  
         BE    P2V120                                                           
         CLM   RF,3,=AL2(1000)                                                  
         BL    *+12                                                             
         CLM   RF,3,=AL2(1300)                                                  
         BL    P2V130                                                           
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB+5(3)                                                 
         CLI   WORK,C'9'                                                        
         BE    P2V140                                                           
         MVC   SRVT1(40),=CL40'CHECK SCRIPT COMPILATION ERROR'                  
         B     P2V200                                                           
P2V100   MVC   SRVT1(40),=CL40'CHECK SCRIPT ABEND'                              
         B     P2V200                                                           
P2V110   MVC   SRVT1(40),=CL40'CHECK SCRIPT NAME OR COMPILATION ERROR'          
         B     P2V200                                                           
P2V120   MVC   SRVT1(50),=CL50'SCRIPT FACPAK PROGRAM CONNECT ERROR'             
         B     P2V200                                                           
P2V130   MVC   SRVT1(37),=CL37'SCRIPT FACPAK SYSTEM NOT OPERATIONAL '           
         LR    RE,RF                                                            
         SH    RE,=H'1000'                                                      
         STC   RE,BYTE                                                          
         MVC   DUB(7),=CL7'SE#(00)'  APPEND SE# TO MESSAGE                      
         GOTO1 =V(HEXOUT),DMCB,BYTE,DUB+4,1,=C'TOG',RR=RELO                     
         MVC   SRVT1+37(7),DUB                                                  
         B     P2V200                                                           
P2V140   MVC   SRVT1(24),=CL24'SCRIPT RUN TIME ERROR - '                        
         MVC   SRVT1+24(30),=CL30'CHECK FASCRIPTQ EQUATE: '                     
         MVI   WORK,C'0'                                                        
         MVC   SRVT1+48(4),WORK                                                 
         MVC   SRVT1+54(8),=CL8'OFFSET: '                                       
         SR    RE,RE                                                            
         ICM   RE,3,SCPOFF                                                      
         CVD   RE,DUB              DISPLACEMENT INTO SCRIPT WHERE IT            
         OI    DUB+7,X'0F'         occurred                                     
         UNPK  SRVT1+62(4),DUB+5(3)                                             
         B     P2V200                                                           
*                                                                               
P2V200   EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,SCPERROR                                                    
         MVC   SRVMSG(60),=CL60'ED/0005 SCRIPT ERROR NUMBER 0000'               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SRVMSG+28(4),DUB+5(3)                                            
         ICM   RF,3,SCPOFF                                                      
         BZ    P2V210              OUTPUT ERROR MESSAGE AND                     
         CVD   RF,DUB              displacement into script where it            
         OI    DUB+7,X'0F'         occurred                                     
         MVI   SRVMSG+33,C'+'                                                   
         UNPK  SRVMSG+35(4),DUB+5(3)                                            
*                                                                               
P2V210   EQU   *                                                                
         BAS   RE,DISPWRK                                                       
         LA    R2,SRVP1H                                                        
         B     EXIT                                                             
*                                                                               
P2V220   EQU   *                                                                
         BAS   RE,DISPWRK                                                       
         LA    R2,SRVP2H                                                        
         TM    IOFLAG,OP_SCRN+OP_FILE  PUT TO SCREEN?                           
         BNZ   OUTPUT                                                           
*                                                                               
         MVC   SRVMSG,=CL60'IT WORKED'                                          
         LA    R2,SRVP1H                                                        
         B     EXIT                                                             
*                                                                               
INPERR   TM    8(R1),X'80'         Test for E.O.F.                              
         BO    *+6                                                              
         DC    H'0'                                                             
INPERR1  MVI   ERRFLAG,3           RECORD NOT FOUND                             
         LA    R2,SRVP2H                                                        
         B     ERREX                                                            
*                                                                               
***********************************************************************         
* Routine to validate input from screen                               *         
***********************************************************************         
*                                                                               
I1VAL    LA    R2,SRVI1H           Point to first input text line               
         LA    R0,6                Number of screen input lines                 
         LA    RE,INPDATA          Buffer for screen input                      
*                                                                               
I1V010   SR    R1,R1                                                            
         ICM   R1,1,5(R2)          Test for data in this line                   
         BZ    I1V020              ..                                           
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)       Move in the data                             
         LA    RE,1(R1,RE)         Bump to the next free data location          
I1V020   IC    R1,0(R2)                                                         
         AR    R2,R1               Next line on screen                          
         BCT   R0,I1V010           Any more lines to validate                   
*                                                                               
         OI    IOFLAG,IP_SCRN      Flag for screen input                        
         B     DOIT                Execute the script                           
*                                                                               
***********************************************************************         
* Validate output type parameter                                      *         
***********************************************************************         
*                                                                               
P3VAL    LA    R2,SRVP3H           Either Worker File or Direct input           
         CLI   SRVP3H+5,0          No input - default to screen                 
         BE    O1VAL                                                            
         CLC   SRVP3(5),=C'SCREEN' Input from screen                            
         BE    O1VAL                                                            
         CLC   SRVP3(4),=C'TEXT'   Input from screen                            
         BE    O1VAL                                                            
         CLC   SRVP3(5),=C'DIRECT' Input from screen                            
         BE    O1VAL                                                            
         CLI   SRVP3,C'*'          Same Worker File as input from               
         BNE   *+12                                                             
         OI    IOFLAG,WKR_SAME     Turn on flag for same file                   
         B     P2VAL                                                            
*                                                                               
         LA    R2,WKEYOUT          Validate this Worker File                    
         USING UKRECD,R2                                                        
         OC    TUSER,TUSER         Get the Terminal Number                      
         BNZ   P3V010                                                           
         TM    TSTAT1,TSTATDDS     Non-DDS must be logged on                    
         BNZ   *+16                                                             
         MVI   ERRFLAG,4                                                        
         LA    R2,SRVP3H                                                        
         B     ERREX                                                            
*&&UK*&& MVC   UKUSRID,=X'0026'    If not connected use DDS1                    
*&&US*&& MVC   UKUSRID,=X'002B'    If not connected use TCH1                    
         B     *+10                                                             
P3V010   MVC   UKUSRID,TUSER       From UTL                                     
         OC    IDNUM,IDNUM         Different USERID (DDS) set?                  
         BZ    *+10                No                                           
         MVC   UKUSRID,IDNUM       Different USERID for DDS allowed             
         MVC   COMPNUM,UKUSRID     Save USERID for compare                      
*                                                                               
         GOTO1 CDATAMGR,DMCB,GFILE,WRKFIL,WKEYOUT,AIOOUT,ABUFFER                
         MVC   WRKFOUT,UKUSRINF    Get this Worker File                         
*                                                                               
         GOTO1 CSCANNER,DMCB,SRVP3H,SCANBLK                                     
         ZIC   R0,4(R1)            Number of params in this field               
         LA    R3,SCANBLK                                                       
         USING SCANBLKD,R3                                                      
         CLI   SC1STLEN,1          S= means search on the sequence #            
         BNE   P3V012              which follows                                
         CLI   SC1STFLD,C'S'       Must be S on it`s own                        
         BNE   P3V012                                                           
*                                                                               
P3V011   OI    WKFLAG,IWKSEQ                                                    
         TM    SC2NDVAL,SCNUMQ                                                  
         BO    *+16                                                             
         MVI   ERRFLAG,2                                                        
         LA    R2,SRVP3H                                                        
         B     ERREX                                                            
         L     R0,SC2NDNUM                                                      
         STCM  R0,3,UKFILENO                                                    
         MVI   UKFLAG,UKFLDAT                                                   
         B     P2V020                                                           
         DROP  R3                                                               
*                                                                               
P3V012   MVI   ERRFLAG,2                                                        
         LA    R2,SRVP3H                                                        
         B     ERREX                                                            
*                                                                               
P3V020   GOTO1 CDATAMGR,DMCB,(X'08',INDEX),=C'WRKFILE',WKEYOUT,AIOOUT, *        
               ATIA                                                             
         CLI   8(R1),0             Read OK?                                     
         BNE   INPERR              No                                           
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',READ),WRKFOUT,WKEYOUT,AIOOUT,ABUFFE*        
               R                                                                
         CLI   8(R1),0             Read file for update - O/P to later          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     P2VAL                                                            
*                                                                               
O1VAL    OI    IOFLAG,OP_SCRN      Flag for output from screen                  
         B     P2VAL                                                            
*                                                                               
OPERR    TM    8(R1),X'80'         Test E.O.F.                                  
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   ERRFLAG,3           Record not Found                             
         LA    R2,SRVP3H                                                        
         B     ERREX                                                            
*                                                                               
***********************************************************************         
* Validate options parameter field                                    *         
***********************************************************************         
*                                                                               
P4VAL    EQU   *                                                                
         LA    R2,SRVP4H                                                        
         CLI   5(R2),0             Input this field?                            
         BE    P3VAL               Nothing to validate                          
         TM    TSTAT1,TSTATDDS     Only DDS Terminals can use another           
         BNZ   *+12                USERID                                       
         MVI   ERRFLAG,4                                                        
         B     ERREX                                                            
*                                                                               
         GOTO1 CSCANNER,DMCB,SRVP4H,(6,SCANBLK)                                 
         ZIC   R0,4(R1)            Number of parameters (1 only supp.)          
         LA    R1,SCANBLK                                                       
         USING SCANBLKD,R1                                                      
         CLI   SC1STLEN,1          U=                                           
         BNE   P3VAL               is the only supported option for now         
         CLI   SC1STFLD,C'U'                                                    
         BNE   P3VAL               Format is U=XXXXXXXX                         
         LA    R2,IDKEY                                                         
         USING CTIREC,R2           Build ID Record Key                          
         MVI   CTIKTYP,CTIKTYPQ    ..                                           
         MVC   CTIKID,SC2NDFLD     ..                                           
         DROP  R1                                                               
         GOTO1 CDATAMGR,DMCB,DMREAD,CTFILE,IDKEY,AIOIN                          
         CLI   8(R1),0             Have we got one?                             
         BNE   P4ERR               No                                           
         L     R2,AIOIN            ID Record in here for now...                 
         LA    R1,CTIDATA          First element in record                      
         DROP  R2                                                               
         XR    R2,R2                                                            
*                                                                               
P4V010   CLI   0(R1),0             E.O.R.                                       
         BNE   *+6                                                              
         DC    H'0'                No X'02' DIE at once                         
         CLI   0(R1),CTDSCELQ      Got the Description element?                 
         BE    P4V020              Yes                                          
         IC    R2,1(R1)            Bump                                         
         LA    R1,0(R2,R1)         ..                                           
         B     P4V010              and try next element                         
*                                                                               
P4V020   MVC   IDNUM,2(R1)         Get new ID number                            
         OI    IOFLAG,NEWUSER      Flag on for different ID                     
         B     P3VAL                                                            
*                                                                               
P4ERR    TM    8(R1),X'90'         E.O.F. or record N.F.                        
         BNZ   *+6                 Yes                                          
         DC    H'0'                                                             
         LA    R2,SRVP4H                                                        
         MVI   ERRFLAG,2           Invalid Field                                
         B     ERREX                                                            
*                                                                               
***********************************************************************         
* Build Script parameter list and execute Script                      *         
***********************************************************************         
*                                                                               
DOIT     EQU   *                                                                
         OC    SCRID,SCRID                                                      
         BNZ   DOI010                                                           
         LA    R2,SRVP1H                                                        
         MVI   ERRFLAG,5                                                        
         B     ERREX                                                            
DOI010   LA    R1,SCRIPTPL         SET UP SCRIPT PARAMETER LIST                 
         USING SCPLD,R1                                                         
         TM    IOFLAG,IP_TEST      Script Test                                  
         BZ    *+8                                                              
         OI    SCPLINDS,SCPLTEST                                                
*                                                                               
         TM    IOFLAG,SCR_SCRN     Script from Screen?                          
         BO    DOI012              YES                                          
*                                                                               
         OI    SCPLINDS,SCPLCTF+SCPLUTB Set Flags                               
         LA    R2,SCRID            A(Name of Script)                            
         STCM  R2,7,SCPLAS+1       Save it                                      
         B     DOI020                                                           
*                                                                               
DOI012   NI    SCPLINDS,255-(SCPLCTF+SCPLUTB) SET FLAGS                         
         LA    RE,TEXT             A(Script Buffer)                             
         STCM  RE,7,SCPLAS+1       Save it                                      
*                                                                               
DOI020   TM    IOFLAG,IP_FILE      File from Screen?                            
         BZ    DOI025                                                           
         LA    RF,INPUTS           A(Input Routine)                             
         ST    RF,SCPLAIR          Save it                                      
         L     R3,AIOIN            A(Input Buffer)                              
         ST    R3,SCPLAID          Save it                                      
         XC    CURDATA,CURDATA                                                  
         BAS   RE,INPY             First Read                                   
         LA    RF,256                                                           
         STH   RF,SCPLLIX          Save length                                  
         B     DOI060                                                           
*                                                                               
DOI025   TM    IOFLAG,IP_SCRN      Input from Screen?                           
         BZ    DOI030              NO                                           
         LA    R2,INPDATA          A(Input Buffer)                              
         ST    R2,SCPLAID          Save it                                      
         B     DOI060                                                           
*                                                                               
DOI030   LA    RF,INPUTR           A(Input Routine)                             
         ST    RF,SCPLAIR          Save it                                      
         L     R3,AIOIN            A(Input Buffer)                              
         USING MYDS,R3             TEMPORARY DSECT FOR WORKER FILES             
         TM    IOFLAG,WKR_SAME     Same Worker File for I/P & O/P?              
         BO    DOI050              Yes                                          
*                                                                               
DOI040   LA    R0,MYDIFF           A(Data in Record)                            
         ST    R0,SCPLAID          Save it                                      
         SR    R0,R3                                                            
         XR    RF,RF               This gets the length of the input            
         ICM   RF,3,MYLEN          data                                         
         SR    RF,R0                                                            
         STH   RF,SCPLLIX          Save length                                  
         B     DOI060                                                           
*                                                                               
DOI050   LA    R0,MYDATA           Same as above, but displacements             
         ST    R0,SCPLAID          are different                                
         SR    R0,R3               ..                                           
         XR    RF,RF               ..                                           
         ICM   RF,3,MYLEN          ..                                           
         SR    RF,R0               ..                                           
         STH   RF,SCPLLIX          ..                                           
*                                                                               
DOI060   TM    IOFLAG,OP_FILE      File to screen?                              
         BZ    DOI065              NO                                           
         LA    RF,OUTPUTS          A(Output routine)                            
         ST    RF,SCPLAOR          Save it                                      
         L     RE,AIOOUT                                                        
         XC    CURFILE,CURFILE                                                  
         ST    RE,SCPLAOD          Save it                                      
         LA    RE,80               L'Buffer - FIXED AT 80                       
         STH   RE,SCPLLOX          Save it                                      
         B     DOI090                                                           
*                                                                               
DOI065   TM    IOFLAG,OP_SCRN      Output to Screen?                            
         BZ    DOI070              NO                                           
         LA    RE,OUTDATA          A(Screen Ouput Buffer)                       
         ST    RE,SCPLAOD          Save it                                      
         LA    RE,L'OUTDATA        L'Buffer                                     
         STH   RE,SCPLLOX          Save it                                      
         B     DOI090                                                           
*                                                                               
DOI070   LA    RF,OUTPUTR          A(Output Routine)                            
         ST    RF,SCPLAOR          Save it                                      
         TM    IOFLAG,WKR_SAME     Same Worker File for I/P and O/P?            
         BO    DOI080              Yes                                          
         L     R3,AIOOUT           Different I/O Area for record read           
         LA    R0,MYDIFF                                                        
         ST    R0,SCPLAOD          Exactly same as above                        
         SR    R0,R3               ..                                           
         XR    RF,RF               ..                                           
         ICM   RF,3,MYLEN          ..                                           
         SR    RF,R0               ..                                           
         STH   RF,SCPLLOX          ..                                           
         B     DOI090              ..                                           
*                                                                               
DOI080   LA    R0,MYOUT                                                         
         ST    R0,SCPLAOD                                                       
         LA    R0,L'MYOUT          Fixed length for O/P for now                 
         STH   R0,SCPLLOX          if in same record                            
         DROP  R3                                                               
*                                                                               
DOI090   LA    R2,MYPARMS          Build my Parameter List                      
         ST    RC,4(R2)            RC     4                                     
         STM   R7,R9,8(R2)         R7-9   8/12/16                               
         ST    R2,SCPLAIP          Save it                                      
         ST    R2,SCPLAOP          ..                                           
*                                  This is passed back if I/O required          
         MVC   SCPLLEB,=AL2(RECLEN)                                             
         L     RF,AERRBUFF         Error buffer                                 
         ST    RF,SCPLAEB                                                       
*                                                                               
         L     RE,VUTL             Scripts supported?                           
         LA    RE,6(RE)                                                         
         TM    TTYPE2-UTLD(RE),TTYPEDUM                                         
         BZ    *+12                                                             
         ICM   RF,15,CSCRIPT       A(FASCRIPT)                                  
         BNZ   DOI100                                                           
         LA    R2,SRVP1H           Error if Scripts not supported               
         MVI   ERRFLAG,2                                                        
         B     ERREX                                                            
*                                                                               
DOI100   GOTO1 (RF),SCRIPTPL       Pass Script for Processing                   
*                                                                               
         LA    R1,SCRIPTPL         Point to script Parameter list               
         SR    RF,RF                                                            
         ICM   RF,3,SCPLERR        Any Errors?                                  
         BZ    DOI120              No                                           
         MVC   SRVMSG,=CL60'ED/0005 SCRIPT ERROR NUMBER 0000'                   
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SRVMSG+28(4),DUB+5(3)                                            
         ICM   RF,3,SCPLDSP                                                     
         BZ    DOI110              Output error message and                     
         CVD   RF,DUB              displacement into script where it            
         OI    DUB+7,X'0F'         occurred                                     
         MVI   SRVMSG+33,C'+'                                                   
         UNPK  SRVMSG+35(4),DUB+5(3)                                            
*                                                                               
DOI110   LA    R2,SRVT1H                                                        
         B     EXIT                                                             
*                                                                               
DOI120   TM    IOFLAG,OP_SCRN+OP_FILE PUT TO SCREEN?                            
         BNZ   OUTPUT              Yes                                          
*                                                                               
DOI130   MVC   SRVMSG,=CL60'IT WORKED...'                                       
         B     EXIT                                                             
*                                                                               
***********************************************************************         
* Routine to output to screen                                         *         
***********************************************************************         
*                                                                               
OUTPUT   LA    R1,SCRIPTPL         Point to script Parameter list               
         LA    R0,6                Number of output lines now                   
         LA    R2,SRVO1H           Point to first output text line              
         L     RE,AERRBUFF                                                      
         OC    0(74,RE),0(RE)                                                   
         BZ    OUT005                                                           
         MVC   8(74,R2),0(RE)                                                   
         LA    R2,SRVO2H           Point to first output text line now          
         LA    R0,5                Number of output lines now                   
*                                                                               
OUT005   XR    RF,RF                                                            
         ICM   RF,3,SCPLLOD        L' of output buffer                          
         BZ    OUT020                                                           
         LA    RE,OUTDATA          A(Output buffer)                             
         DROP  R1                                                               
*                                                                               
OUT010   XR    R1,R1               Get Length of output display line            
         ICM   R1,1,0(R2)                                                       
         SH    R1,=H'8'                                                         
         BNP   OUT020                                                           
         CR    R1,RF                                                            
         BNH   *+6                                                              
         LR    R1,RF               R1=LENGTH TO DISPLAY THIS LINE               
         SH    R1,=H'1'                                                         
         BM    OUT020                                                           
         EX    R1,*+4                                                           
         MVC   8(0,R2),0(RE)       Move in data                                 
         OI    6(R2),X'80'         Transmit                                     
         LA    R1,1(R1)                                                         
         AR    RE,R1               Bump to next output data                     
         SR    RF,R1               Reduce undisplayed length                    
         BNP   OUT020                                                           
         ICM   R1,1,0(R2)                                                       
         AR    R2,R1                                                            
         BCT   R0,OUT010                                                        
*                                                                               
OUT020   LA    R2,SRVT1H                                                        
         B     DOI130                                                           
*                                                                               
***********************************************************************         
* Error exit here                                                     *         
***********************************************************************         
*                                                                               
ERREX    LA    R1,ERRTAB           Table of errors                              
         CLI   0(R1),X'FF'         E.O.T.                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ERRFLAG,0(R1)                                                    
         BE    ERR010                                                           
         LA    R1,L'ERRTAB(R1)                                                  
         B     *-14                                                             
*                                                                               
ERR010   MVC   SRVMSG,1(R1)        Move out message                             
         B     EXIT                                                             
*                                                                               
***********************************************************************         
* Exit point here                                                     *         
***********************************************************************         
*                                                                               
EXIT     OI    6(R2),X'40'         Position cursor                              
         CLI   ERRFLAG,5                                                        
         BE    EXIT1                                                            
         TM    IOFLAG,OP_SCRN+IP_SCRN                                           
         BO    EXIT1                                                            
         SPACE 1                                                                
         L     R2,AERRBUFF         A(Error Buffer)                              
         L     R3,AIOOUT                                                        
         L     R4,ABUFFER                                                       
         LA    R5,WRKFOUT                                                       
         LA    R6,WKEYOUT                                                       
         TM    IOFLAG,WKR_SAME     One file for I/P and O/P?                    
         BZ    ERRLOOP             NO                                           
         L     R3,AIOIN                                                         
         L     R4,ATIA                                                          
         LA    R5,WRKFIN                                                        
         LA    R6,WKEYIN                                                        
         USING MYDS,R3                                                          
ERRLOOP  OC    0(4,R2),0(R2)       Error to pass out?                           
         BZ    EXIT1               No. Leave now                                
         GOTO1 CDATAMGR,DMCB,(X'80',READ),(R5),(R6),(R3),(R4)                   
         TM    8(R1),X'80'         END OF FILE?                                 
         BO    EXIT1               NOT ENOUGH ERROR CARDS                       
         CLI   8(R1),0             OTHERWISE OK?                                
         BE    *+6                                                              
         DC    H'0'                NO - BYE-BYE                                 
         SPACE 1                                                                
         MVC   MYSEQ,=CL6'999999'                                               
         MVC   MYDIFF(ERRLEN),0(R2)                                             
         SPACE 1                                                                
         GOTO1 CDATAMGR,DMCB,WRITE,(R5),(R6),(R3),(R4)                          
         CLI   8(R1),0             BALLS-UP?                                    
         BE    *+6                                                              
         DC    H'0'                YES                                          
         SPACE 1                                                                
         LA    R2,ERRLEN(R2)                                                    
         B     ERRLOOP                                                          
         DROP  R3                                                               
EXIT1    XMOD1 1                                                                
*                                                                               
***********************************************************************         
* INPUT ROUTINE FOR FIRST SCREEN RECORD READ                          *         
***********************************************************************         
*                                                                               
         DS   0D                                                                
INPY     NTR1  ,                                                                
         LA    R1,MYPARMS                                                       
         LA    RF,INPDATA+L'INPDATA                                             
         LH    RE,CURDATA          Displacement to start                        
         LA    RE,INPDATA(RE)                                                   
         LR    R2,RE               Start this time                              
         LA    R0,256                                                           
*                                                                               
INY010   CLI   0(RE),C'º'          End of record delimiter                      
         BE    INY015                                                           
         LA    RE,1(RE)                                                         
         CR    RF,RE               End of buffer?                               
         BNH   INY020              Yes                                          
         BCT   R0,INY010                                                        
*                                                                               
INY015   L     RF,AIOIN            Input buffer                                 
         XC    0(256,RF),0(RF)     Clear it                                     
         SR    RE,R2               Length input this time                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),0(R2)       Move in the data                             
         LA    R2,2(RE,R2)         Next free (allow 1 for º)                    
         LA    RE,INPDATA                                                       
         CLI   0(R2),C'º'          ºº means that`s all                          
         BE    INY020                                                           
         SR    R2,RE                                                            
         STH   R2,CURDATA          Save it                                      
         B     INY030                                                           
*                                                                               
INY020   MVI   0(R1),X'80'         Notify caller no more input                  
*                                                                               
INY030   XIT1  ,                   Back to scrunch                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY WORKER FILE NAME ON SCREEN                                  *         
***********************************************************************         
*                                                                               
         DS   0D                                                                
DISPWRK  NTR1                                                                   
         LA    R3,WKEYIN                                                        
         USING UKRECD,R3                                                        
         LA    R2,IDKEY                                                         
         USING CTIREC,R2           Build ID Record Key                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID+8(2),UKUSRID                                              
         GOTO1 CDATAMGR,DMCB,DMREAD,CTFILE,IDKEY,AIOIN                          
         CLI   8(R1),0             Have we got one?                             
         BE    *+6                                                              
         DC    H'0'                NO                                           
         L     R2,AIOIN            ID Record in here for now...                 
         LA    R1,CTIDATA          First element in record                      
         DROP  R2                                                               
         XR    R2,R2                                                            
*                                                                               
DWRK010  CLI   0(R1),0             E.O.R.                                       
         BNE   *+6                                                              
         DC    H'0'                No X'02' DIE at once                         
         CLI   0(R1),CTDSCELQ      Got the Description element?                 
         BE    DWRK020             YES                                          
         IC    R2,1(R1)            Bump                                         
         LA    R1,0(R2,R1)         ..                                           
         B     DWRK010             AND TRY NEXT ELEMENT                         
*                                                                               
DWRK020  EQU   *                   GET ID NAME                                  
         MVC   SRVI1(12),=CL12'WORKER FILE: '                                   
         MVC   SRVI1+14(10),2(R1)                                               
         MVI   SRVI1+24,C','                                                    
         MVC   SRVI1+25(6),UKSYSPRG                                             
         SR    RF,RF                                                            
         ICM   RF,3,UKFILENO                                                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   SRVI1+31,C','                                                    
         UNPK  SRVI1+32(5),DUB+5(3)                                             
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS & LTORG                                                   *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
BUFFLEN  EQU   14336               Equates for Worker File Lengths              
RECLEN   EQU   4096                                                             
ERRLEN   EQU   60                  Length of an error line                      
*                                                                               
SPACES   DC    CL60' '                                                          
DMREAD   DC    CL7'DMREAD'                                                      
CTFILE   DC    CL7'CTFILE'                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
READ     DC    CL8'READ'                                                        
WRITE    DC    CL8'WRITE'                                                       
ADD      DC    CL8'ADD'                                                         
RANDOM   DC    CL8'RANDOM'                                                      
WRKFIL   DC    CL8'WRKFIL'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
*                                                                               
DBUFFER  DC    Y(BUFFER-SRWORKD)                                                
DERRBUFF DC    Y(ERRBUFF-SRWORKD)                                               
DIOOUT   DC    Y(IOOUT-SRWORKD)                                                 
DIOIN    DC    Y(IOIN-SRWORKD)                                                  
*                                                                               
ERRTAB   DS    0CL61                                                            
         DC    AL1(01),CL60'ED/0001 Missing Input Field'                        
         DC    AL1(02),CL60'ED/0002 Invalid Input Field'                        
         DC    AL1(03),CL60'ED/0003 Invalid Worker File Name'                   
         DC    AL1(04),CL60'ED/0004 Must be logged on'                          
         DC    AL1(05),CL60'ID/0001 Enter Data       '                          
         DC    AL1(99),CL60'ID/0002 Script Displayed - Enter Data'              
         DC    X'FF'                                                            
         EJECT                                                                  
         DROP  RA,RB                                                            
*                                                                               
***********************************************************************         
* INPUT ROUTINE FOR SCREEN RECORD READS                               *         
***********************************************************************         
*                                                                               
         DS   0D                                                                
INPUTS   EQU   *                                                                
         NMOD1 0,**INPS**                                                       
         MVI   0(R1),0                                                          
         L     RC,4(R1)            Get back my registers                        
         LM    R7,R9,8(R1)                                                      
         TM    0(R1),X'80'         Set by me on first read?                     
         BO    IPS030                                                           
*                                                                               
         LA    RF,INPDATA+L'INPDATA                                             
         LH    RE,CURDATA          Displacement to start                        
         LA    RE,INPDATA(RE)                                                   
         LR    R2,RE               Start this time                              
         LA    R0,256                                                           
IPS010   CLI   0(RE),C'º'          End of record delimiter                      
         BE    IPS015                                                           
         LA    RE,1(RE)                                                         
         CR    RF,RE               End of buffer?                               
         BNH   IPS020              Yes                                          
         BCT   R0,IPS010                                                        
*                                                                               
IPS015   SR    RE,R2               Length input this time                       
         BCTR  RE,0                                                             
         L     RF,AIOIN            Input buffer                                 
         XC    0(256,RF),0(RF)     Clear it                                     
         EX    RE,*+4                                                           
         MVC   0(0,RF),0(R2)       Move in the data                             
         LA    R2,2(RE,R2)         Next free (allow 1 for º)                    
         LA    RE,INPDATA                                                       
         CLI   0(R2),C'º'          ºº means that`s all                          
         BE    IPS020                                                           
         SR    R2,RE                                                            
         STH   R2,CURDATA          Save it                                      
         B     IPS030                                                           
*                                                                               
IPS020   MVI   0(R1),X'80'         Notify caller no more input                  
*                                                                               
IPS030   XMOD1 ,                   Back to scrunch                              
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* OUTPUT ROUTINE FOR SCREEN RECORD WRITES                             *         
***********************************************************************         
         DS    0D                                                               
OUTPUTS  EQU   *                                                                
         NMOD1 0,**OUTS**                                                       
         L     RC,4(R1)            Get back my registers                        
         LM    R7,R9,8(R1)                                                      
*                                                                               
         L     R2,AIOOUT                                                        
         LH    RE,CURFILE                                                       
         LA    RE,OUTDATA(RE)                                                   
         LA    RF,OUTDATA+L'OUTDATA                                             
         LA    R0,80               Max output allowed                           
         LA    R3,79(R2)                                                        
OPS010   CLI   0(R3),C' '                                                       
         BH    OPS015                                                           
         BCTR  R3,0                                                             
         BCT   R0,OPS010                                                        
*                                                                               
OPS015   SR    R3,R2               Length to move                               
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,RE),0(R2)       Move it in                                   
         LA    RE,1(R3,RE)                                                      
         CR    RF,RE                                                            
         BH    OPS020              No more space                                
         LA    R2,OUTDATA                                                       
         SR    RE,R2                                                            
         STH   RE,CURFILE          Save next free                               
         B     OPS030                                                           
*                                                                               
OPS020   MVI   0(R1),X'80'         Notify caller of end of data                 
OPS030   XMOD1                                                                  
*                                                                               
***********************************************************************         
* INPUT ROUTINE FOR WORKER FILE READS                                 *         
***********************************************************************         
*                                                                               
         DS   0D                                                                
INPUTR   EQU   *                                                                
         NMOD1 0,**INPUT*                                                       
         MVI   0(R1),0                                                          
         L     RC,4(R1)            Get back my registers                        
         LM    R7,R9,8(R1)                                                      
         LR    R3,R1                                                            
*                                                                               
         TM    IOFLAG,WKR_SAME     Same file for I/P and O/P?                   
         BO    IPR010              Yes                                          
         GOTO1 CDATAMGR,DMCB,IREAD,WRKFIN,WKEYIN,AIOIN,ATIA                     
         CLI   8(R1),0                                                          
         BNE   IPREC                                                            
         B     IPR030                                                           
*                                                                               
IPR010   GOTO1 CDATAMGR,DMCB,(X'80',IREAD),WRKFIN,WKEYIN,AIOIN,ATIA             
         CLI   8(R1),0             Read for update if same file                 
         BNE   IPREC                                                            
         B     IPR020                                                           
*                                                                               
IPREC    TM    8(R1),X'80'         Test EOF                                     
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R3),X'80'         Notify caller                                
         B     IPR030                                                           
*                                                                               
IPR020   L     RF,AIOIN                                                         
         USING MYDS,RF                                                          
         CLC   MYSEQ,=C'999999'                                                 
         BNE   IPR030                                                           
         MVI   0(R3),X'80'         Notify Caller                                
         DROP  RF                                                               
IPR030   XMOD1                                                                  
*                                                                               
         LTORG                                                                  
IREAD    DC    CL8'READ'                                                        
WRKFILI  DC    CL8'WRKFIL'                                                      
*                                                                               
***********************************************************************         
* OUTPUT ROUTINE FOR WORKER FILE WRITES                               *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
OUTPUTR  EQU   *                                                                
         NMOD1 0,**OUTPUT                                                       
         L     RC,4(R1)            Get back my registers                        
         LM    R7,R9,8(R1)                                                      
         LR    R3,R1                                                            
         TM    IOFLAG,WKR_SAME     One file for I/P and O/P?                    
         BZ    OPR010              No                                           
         GOTO1 CDATAMGR,DMCB,OWRITE,WRKFIN,WKEYIN,AIOIN,ATIA                    
         CLI   8(R1),0                                                          
         BE    OPR030                                                           
         DC    H'0'                                                             
*                                                                               
OPR010   GOTO1 CDATAMGR,DMCB,OWRITE,WRKFOUT,WKEYOUT,AIOOUT,ABUFFER              
         CLI   8(R1),0             Write this record                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',OREAD),WRKFOUT,WKEYOUT,AIOOUT,ABUFF*        
               ER                                                               
         CLI   8(R1),0             Need to read next file                       
         BNE   *+8                 to output to here                            
         B     OPR020                                                           
*                                                                               
         TM    8(R1),X'80'         Test EOF                                     
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R3),X'80'         Notify Caller                                
*                                                                               
OPR020   L     RF,AIOOUT                                                        
         USING MYDS,RF                                                          
         CLC   MYSEQ,=C'999999'                                                 
         BNE   OPR030                                                           
         MVI   0(R3),X'80'         Notify Caller                                
         DROP  RF                                                               
OPR030   XMOD1                                                                  
*                                                                               
         LTORG                                                                  
OREAD    DC    CL8'READ'                                                        
OWRITE   DC    CL8'WRITE'                                                       
WRKFILO  DC    CL8'WRKFIL'                                                      
*                                                                               
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
*                                                                               
SRWORKD  DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
RELO     DS    F                                                                
DMCB     DS    6F                  Params for Datamgr                           
MYPARMS  DS    6F                  Params for I/O Routines to FASCRIPT          
AIOIN    DS    A                   1st 4K Record (I/P)                          
AIOOUT   DS    A                   2nd 4K Record (O/P)                          
ATIA     DS    A                   TIA used as first 14K buffer                 
ABUFFER  DS    A                   2nd 14K Buffer                               
AERRBUFF DS    A                   A(Error Buffer)                              
CURDATA  DS    H                                                                
CURFILE  DS    H                                                                
BYTE     DS    X                                                                
SCPERROR DS    XL2                                                              
SCPOFF   DS    XL2                                                              
*                                                                               
SCRIPTPL DS    XL48                FASCRIPT`s Parameter List                    
*                                                                               
IOFLAG   DS    X                   Flag Bytes for I/O used for FASCRIPT         
SCR_SCRN EQU   X'80'               Script comes from screen                     
IP_SCRN  EQU   X'40'               Input comes from screen                      
OP_SCRN  EQU   X'20'               Output goes to screen                        
WKR_SAME EQU   X'10'               Same Worker File used for I/P & O/P          
NEWUSER  EQU   X'08'               Logon under a different USERID (DDS)         
IP_FILE  EQU   X'04'               WORKER FILE TYPE INPT FROM SCREEN            
OP_FILE  EQU   X'02'               WORKER FILE TYPE INPUT TO SCREEN             
IP_TEST  EQU   X'01'               Test Version                                 
*                                                                               
WKFLAG   DS    X                   Flag for Worker File Matches                 
IWKNAME  EQU   X'80'               Input NAME set                               
IWKSEQ   EQU   X'40'               Input SEQUENCE NUMBER set                    
OWKNAME  EQU   X'08'               Output NAME set                              
OWKSEQ   EQU   X'04'               Output SEQUENCE NUMBER set                   
*                                                                               
OWKSEQN  DS    XL2                 Output Number if set                         
IWKSEQN  DS    XL2                 Input Number if set                          
ERRFLAG  DS    X                   Contains Error Number (or 0)                 
WHY      DS    X                                                                
IDNUM    DS    XL2                 Logon ID Number                              
COMPNUM  DS    XL2                 Saved Worker File ID Number                  
WORK     DS    XL17                Used for EDIT                                
WORKL    DS    XL255               LONG WORK AREA                               
SCRID    DS    CL8                 Name of Script                               
KEY      DS    CL25                Script Key                                   
IDKEY    DS    CL25                Used for getting User-ID                     
WRKFIN   DS    CL8                 Name of Input Worker File                    
WRKFOUT  DS    CL8                 Name of Output Worker FIle                   
TEXT     DS    CL280               Buffer for Direct Entry of a Script          
WKEYIN   DS    CL42                Worker File Input Key                        
WKSVUSR  DS    CL2                 WORKER FILE INPUT KEY SAVE USERID            
WKSVFNO  DS    CL2                 WORKER FILE INPUT KEY SAVE FILENO            
INPDATA  DS    CL450               Buffer for Direct Input of Data              
WKEYOUT  DS    CL42                Worker File Output Key                       
OUTDATA  DS    CL450               Buffer for Direct Output of Data             
SCANBLK  DS    CL192               Scanner Work Area                            
RECORD   DS    CL2000              CTFILE I/O Area                              
IOIN     DS    CL(RECLEN)          I/O Area for Worker Files                    
IOOUT    DS    CL(RECLEN)          ..                                           
ERRBUFF  DS    CL(RECLEN)          I/O Area for Error messages (CL60)           
BUFFER   DS    CL(BUFFLEN)         Second I/O Buffer if 2 Worker files          
SRWORKX  DS    0C                                                               
*                                                                               
MYDS     DSECT                     Temporary DSECT for Worker Files             
MYLEN    DS    XL2                 Length of record                             
MYSPR    DS    XL2                 N/Used                                       
MYSEQ    DS    XL6                 ID Number for record from WKTEST             
MYDIFF   DS    0C                  DATA HERE IF DIFFERENT FILES                 
MYOUT    DS    XL20                Output here if same file                     
MYDATA   DS    0C                  Input here if same file                      
*                                                                               
***********************************************************************         
         EJECT                                                                  
*                                                                               
SRSCPFFD DSECT                                                                  
         DS    CL64                                                             
* SRSCPFFD                                                                      
       ++INCLUDE SRSCPFFD                                                       
* FASCRIPTD                                                                     
       ++INCLUDE FASCRIPTD                                                      
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DMWRKFK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFK                                                        
         PRINT ON                                                               
* DMWRKFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFD                                                        
         PRINT ON                                                               
* DMWRKFS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFS                                                        
         PRINT ON                                                               
* DMWRKFW                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFW                                                        
         PRINT ON                                                               
* DMWRKFL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFL                                                        
         PRINT ON                                                               
* DMWRKFB                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFB                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRSCP00   02/06/97'                                      
         END                                                                    
