*          DATA SET RERMP11A   AT LEVEL 223 AS OF 05/01/02                      
*PHASE T81011A,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLSTB                                                               
         TITLE 'T81011 - REPPAK FILE MAINT - INV /ADD/CHA/DIS/DEL/ '            
********************************************************************            
* HISTORY OF CHANGES                                               *            
********************************************************************            
* FEB24/92 (BU ) --- PERMIT A NON-HIT ON INVENTORY DATE TO FIND    *            
*                    THE RECORD WITH THE APPROPRIATE EFFECTIVE     *            
*                    DATE RANGE.                                   *            
*                                                                  *            
* APR29/93 (SKU) --- CHANGE REC2+500 TO REC2+400 (WAS CLOBBERING   *            
*                    STORAGE AFTER END OF REC2)                    *            
*                                                                  *            
* MAR07/95 (BU ) --- FIX INV ADD TRAILER INFO DISAPPEARANCE        *            
*                                                                  *            
* JUN26/95 (BU ) --- FIX 'UT' DEMUP PROBLEM... HA!                 *            
*                                                                  *            
* MAY16/96 (GL ) --- CALL UPVAL TO BUILD FORCED UPGRADE ELEMENT    *            
*                                                                  *            
* JUL02/96 (GL ) --- FIXED BUG IN BUILDING "FROM DATA" ELEMENT     *            
*                                                                  *            
* SEP19/96 (GL ) --- SUPPORT DEMO CALCULATION TAPE PRECISION       *            
*                                                                  *            
* OCT17/97 (GL ) --- SUPPORT FOR COMMUNICATIONS LINK W/ RMP30      *            
*                                                                  *            
* MAR31/98 (GL ) --- SUPPORT WEIGHT-WITH-WEEKS PROFILE             *            
*                                                                  *            
* AUG10/99 (GL ) --- SET SCREEN NUMBER INTO  MYSCRNUM              *            
*                                                                  *            
* JUN28/00 (FD ) --- MODULARIZED TO FIX ADDRESSABILITY ERRORS      *            
*                    (INTRODUCED RELATIVE BRANCHING INSTRUCTIONS)  *            
*                                                                  *            
********************************************************************            
T81011   CSECT                                                                  
         PRINT NOGEN                                                            
MAIN     NMOD1 0,T81011                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
         MVC   AIO,AIO1                                                         
*                                                                               
*  MOVE PROFILE TO LOCAL WORKING STORAGE                                        
         LR    R3,RA                                                            
         AH    R3,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R3                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  R3                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*                                                                               
         XC    INVPRG#,INVPRG#     CLEAR DEM LOOKUP OVERRIDE                    
*                                        (USED IN RERMP04 & 30)                 
*                                                                               
         DS    0H                  SET PRECISION TYPE FOR DEMO CALCS            
         MVI   TAPEOPT,0            BOOK (RTG) BASED DEMO CALCULATIONS          
         TM    RMPPROFS+RMPIMPSB,RMPIMPSA                                       
         BZ    *+8                                                              
         MVI   TAPEOPT,C'Y'         TAPE (IMP) BASED, AS PER PROFILE            
*                                                                               
         DS    0H                  SET WEIGH-WITH-WEEKS FLAG                    
         MVI   WGTWEEK,0                                                        
         TM    RMPPROFS+RMP_WKWB,RMP_WKWA                                       
         BZ    *+8                                                              
         MVI   WGTWEEK,C'Y'         FACTOR WKS INTO WGT, AS PER PROFILE         
*                                                                               
         MVC   RIDBLK(4),=CL4'RID=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK+4(2),AGENCY                                               
*                                                                               
*        THE FOLLOWING 2 FIELDS WERE MOVED INTO WORKING STORAGE FROM            
*        THE CSECT, SO THEY NOW NEED INITIALIZATION.                            
*                                                                               
         MVC   REPFILE,=C'REPFILE ' INITIALIZE FILENAMES FOR DATAMGR            
         MVC   REPDIR,=C'REPDIR  '  AND HELLO SUB CALLS                         
*                                                                               
         EJECT                                                                  
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   MAIN20                                                           
         BRAS  RE,VKEY                                                          
         B     EXIT                                                             
*                                                                               
MAIN20   CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   MAIN30                                                           
         BRAS  RE,VREC                                                          
         B     EXIT                                                             
*                                                                               
MAIN30   CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   MAIN40                                                           
         BRAS  RE,LIST                                                          
         B     EXIT                                                             
*                                                                               
MAIN40   CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   MAIN50                                                           
         BRAS  RE,DKEY                                                          
         B     EXIT                                                             
*                                                                               
MAIN50   CLI   MODE,RECDEL         DELETE                                       
         BE    *+12                                                             
         CLI   MODE,RECREST        AND RESTORE ARE INVALID                      
         BNE   EXIT                                                             
         MVC   RERROR(2),=AL2(INVACT)                                           
         LA    R2,CONACTH                                                       
         J     ERREND                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
         SPACE                                                                  
VKEY     NTR1  BASE=*,LABEL=*                                                   
*              INIT WORK AREA                                                   
         XC    KEY,KEY                                                          
         XC    STAHLD(15),STAHLD                                                
*              ACTION MUST BE ADD                                               
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTCHA                                                    
         BE    VK20                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK20                                                             
         CLI   ACTNUM,ACTREP                                                    
         BE    VK20                                                             
         MVI   ERROR,INVALID                                                    
         J     ERREND                                                           
*              VALIDATE THE STATION                                             
VK20     LA    R2,MTRSSTAH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
         GOTO1 VALISTA                                                          
         MVC   STAHLD,WORK                                                      
         MVI   STAHLD+4,C'T'                                                    
         CLI   WORK+4,C' '                                                      
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+4                                               
         CLI   WORK+40,C' '                                                     
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+40 CHECK SATTELITE                              
         MVC   CSTAT,STAHLD                                                     
         MVC   CCOSCRST,8(R2)                                                   
         SPACE 1                                                                
*                                                                               
*              INVOICE NUMBER                                                   
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    VK100                                                            
         CLI   5(R2),0             OPTIONAL                                     
         BE    VK440                                                            
*                                                                               
VK100    MVI   ERROR,INVALID                                                    
         LA    R2,MTRINVH                                                       
         CLI   5(R2),0                                                          
         JE    ERREND                                                           
         MVC   INVHLD(4),8(R2)                                                  
         OC    INVHLD(4),=4X'40'                                                
         B     VK440                                                            
*                                                                               
*              EFFECTIVE DATE                                                   
*  OPTIONAL                                                                     
VK440    LA    R2,MTREFFH                                                       
*                                                                               
         LA    R5,21               LENGTH OF KEY CHECK (WITHOUT DATE)           
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK560                                                            
*                                                                               
         LA    R5,24               LENGTH OF KEY CHECK (WITH DATE)              
*                                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
         XC    DTEHLD,DTEHLD                                                    
         XC    DTEHLD2(4),DTEHLD2                                               
*                                                                               
         LA    R2,MTREFFH          EDIT DATES                                   
         CLI   5(R2),0                                                          
         BE    VK560                                                            
         GOTO1 ANY                                                              
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK      START DATE                       
         OC    DMCB(4),DMCB                                                     
         JZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,DTEHLD)                                  
         GOTO1 DATCON,DMCB,(0,WORK),(2,DTEHLD2)                                 
*                                                                               
VK560    MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,DTEHLD                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKXIT                                                            
         CLI   ACTNUM,ACTREP                                                    
         BE    VKXIT                                                            
*                                                                               
         LA    R2,MTRSSTAH                                                      
         GOTO1 GETINV                                                           
         L     R6,AIO                                                           
*!!!                                                                            
         MVC   TIMECHG,RINVTCHG     TIME CHANGE                                 
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         BRAS  RE,DKEY                                                          
*                                                                               
VKXIT    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DKEY     NTR1  BASE=*,LABEL=*                                                   
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
*        LA    R2,MTRSSTAH         STATION                                      
*        MVC   8(4,R2),RINVKSTA                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,MTRINVH          INVENTORY                                    
         MVC   8(4,R2),RINVKINV                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,MTREFF           EFFECTIVE DATE                               
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,0(R2))                               
         OI    MTREFFH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*  PROGRAM                                                                      
DKEY20   LA    R2,MTRPROGH                                                      
         USING RIPGELEM,R4                                                      
         PRINT GEN                                                              
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',(R6)),0                         
         PRINT NOGEN                                                            
         CLI   12(R1),0                                                         
         BNE   DKEY80                                                           
         L     R4,12(R1)                                                        
*                                                                               
DKEY40   ZIC   R1,RIPGLEN                                                       
         S     R1,=F'3'                                                         
         LA    R3,RIPGNAME                                                      
         EX    R1,*+8              MOVE TO SCREEN                               
         B     *+10                                                             
         MVC   8(0,R2),0(R3)                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   RE,RIPGLEN                                                       
         AR    R4,RE                                                            
*                                                                               
DKEY80   LA    R2,MTRDYTMH                                                      
         XC    MTRDYTM,MTRDYTM                                                  
         USING RIDTELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',(R6)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
*                                                                               
DKEY100  XC    WORK,WORK                                                        
         GOTO1 UNDAY,DMCB,RIDTDAY,MTRDYTM           DAY                         
         LA    RE,17                                                            
         LA    R3,MTRDYTM                                                       
*                                                                               
DKEY112  CLI   0(R3),X'40'                                                      
         BNH   DKEY114                                                          
         LA    R3,1(R3)                                                         
         BCT   RE,DKEY112                                                       
         DC    H'00'                                                            
DKEY114  MVI   0(R3),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R3))       TIME                        
         OC    RIDTTIME+2(2),RIDTTIME+2                                         
         BNZ   DKEY180                                                          
*-- NO END TIME MOVE ,B AFTER START TIME                                        
         LA    RE,8(R2)                                                         
         LA    RF,19                                                            
DKEY120  CLI   0(RE),X'40'                                                      
         BNH   DKEY140                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,DKEY120                                                       
         DC    H'0'                                                             
DKEY140  MVC   0(2,RE),=CL2',B'                                                 
*                                                                               
DKEY180  OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   RE,RIDTLEN                                                       
         AR    R4,RE                                                            
         DROP  R4                                                               
*                                                                               
DKXIT    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
*              TRANSFER DATA                                                    
         SPACE 1                                                                
VREC     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 CHKLOCK                                                          
         NI    TWASTAT1,X'FF'-VALKEYOK   FORCE VALKEY MODE                      
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,(X'30',0),(RA)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT81030,DMCB        A(T81030)                                    
         SPACE 1                                                                
         L     RE,AIO                                                           
         MVC   TRSVKEY,0(RE)       SAVE HEADER'S KEY                            
         SPACE                                                                  
         LA    R2,MTRBOK1H                                                      
         SPACE                                                                  
VR60     DS    0H                                                               
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   VR70                                                             
         MVC   8(17,R2),SVSINV+8                                                
         MVC   5(1,R2),SVSINV+5                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
VR70     CLI   5(R2),0                                                          
         BE    VREX                END OF EDIT                                  
         MVI   BYTE4,1                                                          
         CLI   8(R2),C'*'          SKIP EDIT OF LINE IF BOOKS START             
         BE    VR80                WITH AN ASTERISK                             
         MVC   DEMEDIA(8),INVMED   RESTORE HEADER'S STATION                     
         BRAS  RE,DYTIMSET                                                      
         BAS   RE,DATAREC                                                       
         MVC   AIO,AIO1            RESET AIO                                    
*        CLI   ERROR,0                                                          
*        BNE   VR100               ERROR ENCOUNTERED                            
*                                                                               
VR80     DS    0H                                                               
         ZIC   R0,BYTE4                                                         
         LA    R2,LINELEN(R2)      BUMP POINTER FOR EACH                        
         BCT   R0,*-4              CONTINUATION LINE INVOLVED                   
         LA    RF,MTRLSTH                                                       
         CR    R2,RF               END OF SCREEN REACHED                        
         BL    VR60                NO                                           
         B     VREX                YES                                          
*                                                                               
* ERROR ROUTINE - PUT A STAR IN FIRST POSITION OF BOOKS TO STOP DOUBLE          
* EDITS ON LINES ABOVE ERROR LINE                                               
*                                                                               
VR100    DS    0H                                                               
         LA    R1,MTRBOK1H                                                      
*                                                                               
VR120    CR    R1,R2               REACHED ERROR LINE                           
         BE    VR180               YES                                          
         CLI   8(R1),C'*'          STAR THERE FROM PREVIOUS ERROR               
         BE    VR140               YES                                          
*                                                                               
         MVC   WORK(L'MTRBOK1),8(R1)                                            
         MVI   8(R1),C'*'          STAR IN BOOKS FIELD                          
         MVC   9(L'MTRBOK1-1,R1),WORK                                           
         OI    6(R1),X'80'         TRANSMIT                                     
*                                                                               
VR140    DS    0H                                                               
         LA    R1,LINELEN(R1)                                                   
         B     VR120                                                            
*                                                                               
VR180    DS    0H                                                               
         B     VREX                                                             
*                                                                               
VREX     DS    0H                                                               
         BRAS  RE,GOLTRANS                                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              EDIT  TRANSFER DATA                                              
         SPACE 1                                                                
* R2 POINTS TO BOOKS HEADER                                                     
* R4 AT ENTRY POINTS TO HEADER RECORD                                           
*                                                                               
DATAREC  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         USING REINVREC,R4                                                      
         XC    TRFNOVER,TRFNOVER                                                
*                                                                               
         NI    MYFLAG,X'FF'-TNSDFTP                                             
*                                                                               
         TM    RINVATD,X'40'       TRANSFER DEFAULT=TP?                         
         BZ    *+8                                                              
         OI    MYFLAG,TNSDFTP                                                   
*                                                                               
         L     R5,AIO2             WORKING STORAGE FOR TRANSFER DATA            
         ST    R5,ATRANS                                                        
*                                                                               
         MVC   DEMSTA,RINVKSTA     DEFAULT DEMO STATION                         
*                                                                               
         XC    TRBKLIST,TRBKLIST                                                
         GOTO1 =V(BKLST),DMCB,(R2),(C'B',TRBKLIST),BOOKVAL,SCANNER,    +        
               RR=YES                                                           
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         JE    ERREND                                                           
         MVC   TRBKCNT,DMCB        NUMBER OF BOOK ENTRIES                       
*                                                                               
         ZIC   R5,TRBKCNT                                                       
         MHI   R5,2                NUMBER OF BOOKS TO CHECK                     
*                                                                               
         LA    R6,TRBKLIST         LIST OF BOOKS                                
*                                                                               
DATAR1   DS    0H                  CHECK IF BOOK DATE <= TODAY'S DATE           
*                                                                               
         BRAS  RE,GETBKTYP         GET BOOK TYPE IN EBCDIC                      
*                                                                               
         GOTO1 VALBKDAT,DMCB,TODAYBIN,1(R6),BKTYPE                              
         LA    R6,4(R6)            NEXT BOOK                                    
         BCT   R5,DATAR1                                                        
*                                                                               
DATAR2   EQU   *                                                                
         ST    R2,THISLINE         SAVE LINE START                              
         LA    R2,CODEH(R2)        VALIDATE PROGRAM CODE                        
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   DATAR2A                                                          
         MVC   8(2,R2),SVSINV+CODE                                              
         MVC   5(1,R2),SVSINV+CODEH+5                                           
         OI    6(R2),X'80'                                                      
*                                                                               
DATAR2A  BAS   RE,MOVE                                                          
         LA    RE,CODETAB                                                       
         LA    R1,CODES                                                         
         CLC   WORK(2),0(RE)                                                    
         BE    DATAR3                                                           
         LA    RE,L'CODETAB(RE)                                                 
         BCT   R1,*-14                                                          
         LA    RE,MONTAB           NOT A CODE, LOOK FOR A MONTH/YEAR            
         LA    R1,MONTHSS                                                       
         CLC   WORK(1),0(RE)                                                    
         BE    *+16                VALID MONTH CODE                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-14                                                          
         J     ERREND                                                           
         SPACE                                                                  
         CLI   WORK+1,C'0'         NOW LOOK FOR A NUMBER                        
         JL    ERREND                                                           
         CLI   WORK+1,C'9'                                                      
         JH    ERREND                                                           
         MVC   INVCODE,WORK                                                     
         MVI   INVCDCTL,PRO+INV                                                 
         B     DATAR5                                                           
         SPACE                                                                  
DATAR3   EQU   *                                                                
         MVC   INVCODE,WORK                                                     
         MVC   INVCDCTL,2(RE)      CONTROL BITS                                 
         SPACE                                                                  
DATAR5   EQU   *                                                                
         MVI   INVTYP,C'P'         DEFAULT-FROM DEMO FILES                      
         L     R2,THISLINE                                                      
         LA    R2,TYPEH(R2)                                                     
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   DATAR5A                                                          
         MVC   8(1,R2),SVSINV+TYPE                                              
         MVC   5(1,R2),SVSINV+TYPEH+5                                           
         OI    6(R2),X'80'                                                      
*                                                                               
DATAR5A  CLI   5(R2),0                                                          
         BE    *+18                                                             
         CLI   5(R2),1             VALIDATE TYPE                                
         JNE   ERREND                                                           
         MVC   INVTYP,8(R2)                                                     
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BE    DATAR6                                                           
         CLI   INVTYP,C'P'                                                      
         JNE   ERREND                                                           
         TM    INVCDCTL,PRO        CODE CONSISTENCY WITH 'P'                    
         JZ    ERREND                                                           
         SPACE                                                                  
DATAR6   EQU   *                                                                
         MVI   BYTE4,1             NO CONTINUATION                              
         L     R2,THISLINE                                                      
         LA    R2,FROMH(R2)                                                     
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   DATAR6A                                                          
         MVC   8(27,R2),SVSINV+FROM                                             
         MVC   5(1,R2),SVSINV+FROMH+5                                           
         OI    6(R2),X'80'                                                      
*                                                                               
DATAR6A  CLI   5(R2),0             TEST IF FROM DETAILS INPUT                   
         BNE   DATAR8              YES                                          
         CLI   INVTYP,C'I'                                                      
         BE    DATAR6AB            FOR BOOK TRANSFER                            
         CLC   INVCODE,=C'PR'      CODE 'PR' REQUIRES PURE NUMBER               
         BNE   DATAR6AB                                                         
         MVC   RERROR,=AL2(MISINP)                                              
         J     ERREND2                                                          
DATAR6AB MVI   INVNO,1             NO-BUILD A DUMMY ENTRY                       
*                                                                               
         L     R5,AIO3                                                          
         ST    R5,INVLIST                                                       
         L     R3,ATRANS           TRANSFER DATA STORAGE                        
         XC    0(250,R3),0(R3)     ALSO BUILD A DUMMY HEADER AND                
         MVI   0(R3),250           DATA W DEFAULT DETAILS                       
         LA    R6,8(R3)            POINT R6 AT DATA START                       
         USING INVLD,R5                                                         
         XC    INVLREC,INVLREC     HAS NOT BEEN INPUT BY USER                   
         MVI   INVLWT,1                                                         
         MVC   INVLFLE,INVTYP      TAKE FILE FROM TYPE                          
         CLI   INVLFLE,C'I'        TEST FOR INVENTORY                           
         BE    DATAR7                                                           
*&&DO                                                                           
         OI    INVLTYP,X'60'       SET DAY/TIME BITS                            
*&&                                                                             
                                                                                
         OI    INVLTYP,X'40'       THIS ENTRY IS THE 1ST DAY/TIME               
                                                                                
         LA    RF,DYTIMTAB         DAY/TIMES ARE STORED IN THIS TABLE           
DATAR6AC DS    0H                                                               
         MVC   INVLDAY,0(RF)        DAY                                         
         MVC   INVLSTIM(4),1(RF)    START & END TIMES                           
         CLI   5(RF),0             ANY MORE DAY/TIME IN TABLE?                  
         BE    DATAR6AF             NOPE                                        
                                                                                
         DS    0H                   YES, THERE ARE MULTIPLE DAY/TIMES           
         MVC   (INVLREC+L'INVLREC)(L'INVLREC),INVLREC                           
         NI    INVLTYP+L'INVLREC,X'FF'-X'60'                                    
         LA    R5,L'INVLREC(R5)                                                 
         LA    RF,5(RF)                                                         
         B     DATAR6AC                                                         
                                                                                
DATAR6AF DS    0H                                                               
         OI    INVLTYP,X'20'       THIS ENTRY IS THE LAST DAY/TIME              
                                                                                
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
*                                                                               
         MVC   INVDAYS(5),2(RE)    DAY TIME                                     
*&&DO                                                                           
         MVC   INVLDAY,2(RE)       USE HEADER'S DAY/TIME IN                     
         MVC   INVLSTIM(4),3(RE)  DUMMY ENTRY FOR BOOK TRANSFER                 
*&&                                                                             
         CLC   INVTIM+2(2),=C'CC' TEST FOR TO CONCLUSION                        
         BNE   DATAR6B                                                          
*&&DO                                                                           
         SR    R1,R1               ADD 2 HOURS TO START                         
         ICM   R1,3,INVLSTIM                                                    
         AHI   R1,200                                                           
         CHI   R1,2400             TEST FOR RUN PAST MIDNIGHT                   
         BNH   *+8                                                              
         SHI   R1,2400                                                          
         STCM  R1,3,INVLSTIM+2     SET END TIME                                 
*&&                                                                             
         B     DATAR6B                                                          
         SPACE 1                                                                
DATAR6B  DS    0H                                                               
         GOTO1 UNDAY,DMCB,INVDAYS,(R6)                                          
         CLI   0(R6),C' '          TEST FOR END OF DAY EXPRESSION               
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         MVI   0(R6),COMMA         INSERT COMMA AFTER IT                        
         LA    R6,1(R6)                                                         
*                                                                               
         GOTO1 UNTIME,DMCB,INVTIM,(R6)                                          
         CLI   0(R6),C' '          FIND END OF TIME EXPRESSION                  
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         OC    INVTIM+2(2),INVTIM+2 TEST FOR BREAK CODE                         
         BNZ   *+14                                                             
         MVC   0(2,R6),=C',B'                                                   
         LA    R6,2(R6)            BUMP OUTPUT POINTER                          
         LA    R1,8(R3)                                                         
         SR    R6,R1               FIND LENGTH OF DATA                          
         STC   R6,5(R3)                                                         
         B     DATAR15                                                          
         SPACE 1                                                                
DATAR7   MVI   INVLTYP,X'80'                                                    
         LA    RE,TRSVKEY                                                       
         MVC   INVLNUMB,RINVKINV-RINVKEY(RE)                                    
         MVC   INVLDATE,RINVKSTD-RINVKEY(RE)                                    
         ZIC   R1,INVLNUMB         QUARTER HOUR                                 
         CVD   R1,DUB                                                           
         UNPK  0(2,R6),DUB+6(2)                                                 
         OI    1(R6),X'F0'                                                      
         MVC   2(1,R6),INVLNUMB+1  DAY CODE                                     
         LA    R6,3(R6)                                                         
         CLI   INVLNUMB+2,C'0'     TEST FOR LENGTH OR SPECIAL CODE              
         BE    *+14                                                             
         MVC   0(1,R6),INVLNUMB+2                                               
         LA    R6,1(R6)                                                         
*                                                                               
         MVI   0(R6),COMMA         COMMA AFTER INVENTORY NUMBER                 
         LA    R6,1(R6)                                                         
         GOTO1 DATCON,DMCB,(3,INVLDATE),(5,(R6))                                
         LA    R6,8(R6)                                                         
         LA    R1,8(R3)                                                         
         SR    R6,R1               FIND DATA LENGTH                             
         STC   R6,5(R3)                                                         
         B     DATAR15                                                          
         DROP  R5                                                               
         SPACE                                                                  
DATAR8   EQU   *                                                                
         L     R5,ATRANS                                                        
         XC    0(250,R5),0(R5)                                                  
         MVI   0(R5),250           BUILD DUMMY HEADER FIELD                     
         ZIC   RF,5(R2)                                                         
         LR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),8(R2)                                                    
         STC   RF,5(R5)            INPUT LENGTH                                 
         SPACE                                                                  
         L     RE,THISLINE         POINT RE TO NEXT LINE                        
DATAR9   LA    RE,LINELEN(RE)      AND LOOK FOR A CONTINUATION LINE             
         LA    RF,MTRLSTH                                                       
         CR    RE,RF                                                            
         BNL   DATAR12             PAST LAST DATA LINE.                         
         CLI   5(RE),0             BOOKS,TYPE,CODE ON NEXT LINE                 
         BNE   DATAR12             MUST BE EMPTY AND FROM MUST                  
         CLI   TYPEH+5(RE),0       HAVE INPUT TO BE A                           
         BNE   DATAR12             CONTINUATION LINE.                           
         CLI   CODEH+5(RE),0                                                    
         BNE   DATAR12                                                          
         CLI   FROMH+5(RE),0                                                    
         BE    DATAR12                                                          
         SPACE                                                                  
DATAR10  EQU   *                                                                
         AI    BYTE4,1             INCREMENT THE LINE COUNTER                   
         XR    R3,R3                                                            
         LA    R3,FROMH(RE)        POINT R3 AT FROM DETAILS                     
         ZIC   RF,5(R5)                                                         
         LA    R6,7(R5,RF)         POINT R6 AT LAST INPUT BYTE                  
         CLI   0(R6),COMMA                                                      
         BE    *+16                                                             
         LA    R6,1(R6)            POINT R6 AT NEXT BYTE AND                    
         MVI   0(R6),COMMA         INSERT THE COMMA                             
         LA    RF,1(RF)                                                         
         SPACE                                                                  
         LA    R6,1(R6)            BUMP TO FIRST AVAILABLE BYTE                 
         ZIC   R1,5(R3)                                                         
         AR    RF,R1               TOTAL INPUT                                  
         STC   RF,5(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE DATA TO STRING                          
         B     DATAR9              EDIT NEXT LINE DATA                          
         MVC   0(0,R6),8(R3)                                                    
         SPACE                                                                  
DATAR12  EQU   *                                                                
         L     RE,AIO3             AIO3 CONTAINS INVLIST                        
         ST    RE,INVLIST                                                       
*  BUFF IS TO BE USED AS A SCANNER BLOCK                                        
         GOTOR INVLST,DMCB,(R5),BUFF                                            
         SPACE 2                                                                
* GENERATE DATA RECORDS IN A LOOP USING BOOK LIST.                              
* R3 WILL USED AS A COUNTER OF BOOK LIST ENTRIES.  ERROR                        
* MESSAGE NUMBERS MUST BE LOADED ONLY UPON ERROR EXIT                           
*                                                                               
DATAR15  DS    0H                                                               
         MVI   TRMODE,C'I'         INITIALIZE FOR BUFFERING AND                 
         GOTO1 =A(BUFFER),DMCB,(RC),RR=YES                                      
         ZIC   R3,TRBKCNT          COUNT OF ENTRIES IN BOOK LIST                
         LA    R5,TRBKLIST         R5 POINTS TO ENTRY                           
         USING BKLSTD,R5                                                        
         SPACE                                                                  
DATAR16  EQU   *                                                                
         MVC   INVSRC,0(R5)        BOOKVAL BIT OF FROM BOOK                     
         MVC   INVFBK,1(R5)        FROM BOOK                                    
         MVC   INVBTYPE,3(R5)      FROM BOOK TYPE                               
         MVC   INVTOBK(4),4(R5)    TO BOOK                                      
         CLC   INVTOBK+1(2),=C'AM'  TEST FOR SAME                               
         BNE   *+10                                                             
         MVC   INVTOBK(4),INVSRC   SET TO BOOK EQUAL TO FROM BOOK               
         CLI   OTOBTYP,0           NO OUTPUT BKTYPE?                            
         BE    DATAR16A                                                         
         CLC   OTOBTYP,OFRBTYP      SAME BKTYPE AS FROM?                        
         BE    DATAR16A                                                         
         TM    OTOBIT,X'20'        IF ESTIM BK OR PROJ BK -> ERROR              
         BO    *+12                                                             
         TM    OTOBIT,X'04'                                                     
         BNO   DATAR16A                                                         
         CLI   OFRBTYP,0                                                        
         BNE   *+14                                                             
         MVC   INVBTYPE,OTOBTYP    SET SOURCE BKTYPE=DESTIN BKTYPE              
         B     DATAR16A                                                         
         L     R2,THISLINE         DON'T ALLOW MIXED UP BKTYPES                 
         MVI   ERROR,INVALID                                                    
         J     ERREND                                                           
*                                                                               
         SPACE                                                                  
DATAR16A CLC   INVCODE,=C'TP'      DO NOT PERMIT CODE 'TP' IF:                  
         BNE   DATAR17                                                          
         CLI   STAHLD+4,C'H'      NHTI STATION?                                 
         BNE   DATAR16C               OR                                        
         L     R2,THISLINE                                                      
         LA    R2,CODEH(R2)                                                     
         MVC   RERROR,=AL2(NASTN)                                               
         J     ERREND2                                                          
*                                                                               
DATAR16C CLC   INVFBK,=X'520A'     FROM BOOK PRIOR TO OCT82                     
         BNL   DATAR17                                                          
         L     R2,THISLINE                                                      
         LA    R2,CODEH(R2)                                                     
         MVI   ERROR,INVALID                                                    
         J     ERREND                                                           
         SPACE 1                                                                
DATAR17  LA    RE,BUFF             CLEAR AND BUILD ONE DATA                     
         A     RE,=F'4000'                                                      
*        LA    RE,3000(RE)                                                      
         LR    R4,RE               RECORD FOR EACH BOOK ENTRY IN                
         LA    RF,2000             REC.                                         
         SR    R1,R1                                                            
         MVCL  RE,R0               ZERO REC.                                    
         ST    R4,AIO                                                           
         SPACE                                                                  
         MVC   RINVKEY,TRSVKEY     MOVE IN HEADER'S KEY                         
         MVC   RINVKSRC(3),INVTOBK                                              
*                                                                               
         XC    IBLK,IBLK           INPUT BLOCK:                                 
         MVC   IBLK+3(1),INVTOBK     BOOKVAL BITS                               
         MVC   IBLK+4(1),INVTOBK+3   BOOKTYPE                                   
         GOTO1 VGETKSRC,DMCB,(C'B',IBLK),OBLK                                   
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BE    *+16                INVALID BOOK                                 
         L     R2,THISLINE                                                      
         MVI   ERROR,INVALID                                                    
         J     ERREND                                                           
         MVC   RINVKSRC,OBLK+2     SET RINVKSRC                                 
*                                                                               
DATAR17E MVC   RINVLEN,=H'35'      SET LENGTH FOR VIRGIN RECORD                 
*                                                                               
         CLC   INVCODE,=C'ES'      ESTIMATED BOOK?                              
         BE    DATAR17F                                                         
         CLC   INVCODE,=C'PJ'      PROJECTED BOOK?                              
         BE    DATAR17F                                                         
*                                                                               
         TM    MYFLAG,TNSDFTP      TRANSFER DEFAULT=TP?                         
         BZ    *+14                                                             
         MVC   INVCODE,=C'TP'                                                   
         MVI   INVCDCTL,TP                                                      
*                                                                               
DATAR17F DS    0H                                                               
         XC    ACMMNCTE,ACMMNCTE   NOTHING TO TELL RMP30 (YET)                  
         GOTO1 VT81030,DMCB,(RC)                                                
         SPACE                                                                  
         CLI   INVBAD,0                                                         
         BE    DATAR18                                                          
         ZIC   R3,INVBAD           ERROR MESSAGE FROM DEMO MODULE               
         L     R2,THISLINE         POSITION CURSOR AT FROM FIELD                
         LA    R2,FROMH(R2)                                                     
         MVC   ERROR,INVBAD                                                     
         J     ERREND                                                           
         SPACE                                                                  
DATAR18  DS    0H                                                               
         MVC   HALF,27(R4)         REPAIR RECORD LENGTH AFTER                   
         LH    RE,HALF             DEMO MODULES                                 
         BCTR  RE,0                                                             
         STCM  RE,3,27(R4)                                                      
         SPACE 1                                                                
*                                                                               
         BAS   RE,BLDCE            BUILD DAY TIME ELEMENTS                      
*                                                                               
         SPACE                                                                  
         XC    WORK2(200),WORK2    BUILD TRANSFER FROM ELEMENT                  
         LA    RE,WORK2            SO DEMUP WILL HAVE FROM SRC/BOOK             
         USING RINVFREL,RE                                                      
         MVI   RINVFRCD,X'03'                                                   
         MVC   RINVFRST,DEMSTA                                                  
         MVC   RINVFRBK,INVSRC                                                  
         MVC   RINVFRTY,INVTYP                                                  
         MVI   RINVFRPR,C'A'                                                    
         MVC   RINVFRBT,INVBTYPE                                                
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BNE   *+10                                                             
         MVC   RINVFRBT,INVFRBT    USE BK TYPE PASSED BY T81030                 
*                                                                               
         ZIC   R0,BYTE4            R0 = # OF LINES                              
         LA    R2,RINVFRDT                                                      
         L     RF,THISLINE         A(THIS LINE)                                 
         LA    RF,FROMH(RF)        BUMP TO 'FROM DETAIL' FIELD                  
*                                                                               
*    TRANSFER BLOCK HAS BEEN CREAMED BY T81030 USE.  AS THERE IS                
*        NOTHING THERE, THE ORIGINAL LINE IS NOW USED.                          
*                                                                               
****>>>  L     RF,ATRANS           POINT TO TRANSFER BLOCK                      
*                                                                               
DATA19B  DS    0H                                                               
         ZICM  R1,5(RF),(1)        VARIABLE DATA LENGTH                         
         BZ    DATA19D              NO DATA HERE                                
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),8(RF)      FROM DETAILS                                 
         LA    R2,1(R2,R1)                                                      
                                                                                
DATA19D  DS    0H                                                               
         BCT   R0,*+8              DECREMENT LOOP COUNTER                       
         B     DATA19F                                                          
                                                                                
         CLI   0(R2),COMMA         IF THERE IS NO COMMA,                        
         BE    *+12                                                             
         MVI   0(R2),COMMA          INSERT ONE                                  
         LA    R2,1(R2)                                                         
         LA    RF,LINELEN(RF)      BUMP TO NEXT CONTINUATION LINE               
         B     DATA19B                                                          
                                                                                
DATA19F  DS    0H                                                               
         LA    R0,RINVFRDT                                                      
         SR    R2,R0                        R2 = L(FROM DATA)                   
         LA    R2,RINVFRDT-RINVFREL(R2)     FIND EL LEN                         
         STC   R2,RINVFRLN                                                      
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,(R4)),WORK2,0                       
         DROP  RE                                                               
         SPACE 1                                                                
         LA    RE,WORK             BUILD CODE ELEMENT                           
         USING RINVCEL,RE                                                       
         XC    WORK,WORK                                                        
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,INVCODE                                                 
         TM    INVCDCTL,TP         FOR TIME PERIOD TRANSFERS WHERE              
         BZ    *+18                AUTOMATIC FOOTNOTING IS SUPPRESSED,          
         CLI   TRFNOVER,YES        CLEAR THE CODE ON RECORD                     
         BNE   *+10                                                             
         MVC   RINVCODE,SPACES                                                  
         TM    INVTOBK,X'20'       ESTIMATED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'E'                                                    
         SPACE 1                                                                
         TM    INVTOBK,X'04'       PROJECTED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'P'                                                    
         SPACE 1                                                                
         TM    INVTOBK,X'02'       SPECIAL SURVEY BOOK TEST                     
         BZ    *+8                                                              
         MVI   RINVCSET,C'S'                                                    
         SPACE 1                                                                
         L     R1,THISLINE         POINT R1 AT FROM FIELD                       
         LA    R1,FROM(R1)                                                      
         CLI   0(R1),C'+'                                                       
         BNE   *+8                                                              
         OI    RINVCTYP,X'80'      ADDED DEMOS                                  
         OC    RINVCTYP,INVIND     CUMULATIVE INDICATORS                        
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,(R4)),WORK,0                        
         DROP  RE                                                               
         SPACE 1                                                                
DATAR20  DS    0H                                                               
         L     R2,THISLINE                                                      
         LA    R2,UPGH(R2)                                                      
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   DATAR20A                                                         
         MVC   8(16,R2),SVSINV+UPG                                              
         MVC   5(1,R2),SVSINV+UPGH+5                                            
         OI    6(R2),X'80'                                                      
*                                                                               
DATAR20A CLI   5(R2),0                                                          
         BE    DATAR24                                                          
*                                                                               
* CODE FOR USER INPUT UPGRADE                                                   
*                                                                               
         XC    WORK,WORK                                                        
         L     R7,ACOMFACS                                                      
         GOTO1 UPVAL,DMCB,(1,(R2)),(C'Y',WORK),(R7)                             
         CLI   DMCB,1                                                           
         BE    *+14                                                             
         MVC   RERROR,=AL2(BADUPGR) INVALID UPGRADE                             
         J     ERREND2                                                          
         SPACE                                                                  
         L     RE,THISLINE          UPGRADES ARE NO GOOD FOR COMBOS             
         LA    RE,FROM(RE)                                                      
         CLI   0(RE),C'+'                                                       
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         J     ERREND                                                           
         SPACE 1                                                                
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         CLI   OTOBTYP,0           FOR EST & PROJ BKS W/OUTPUT BKTYPES,         
         BE    DATAR20B            --UPGRADES PULL DATA FROM BKTYPE BKS         
         TM    OTOBIT,X'20'        ESTIM BK?                                    
         BO    *+12                                                             
         TM    OTOBIT,X'04'        PROJ BK?                                     
         BNO   DATAR20B                                                         
         CLI   RAVLNTYP,3          HUT/PUT UPGRADE                              
         BE    *+12                                                             
         CLI   RAVLNTYP,6          HPT UPGRADE                                  
         BNE   DATAR20B                                                         
         CLC   RAVLNOP1,=H'500'    IF FIELD > 500 --> THIS IS A BK              
         BH    *+14                                                             
         CLC   RAVLNOP2,=H'500'                                                 
         BL    DATAR20B                                                         
         CLC   RAVLNBT,OTOBTYP                                                  
         BE    DATAR20B                                                         
         CLI   RAVLNBT,0                                                        
         BNE   *+14                                                             
         MVC   RAVLNBT,OTOBTYP     SET FROM BKTYPE IN UPGRADE                   
         B     DATAR20B                                                         
         MVI   ERROR,INVALID       IF UPGD BKTYPE <> KEY BKTYP ->ERROR          
         J     ERREND                                                           
*                                                                               
DATAR20B CLI   RAVLNTYP,3          ONLY PERMIT ONE BOOK OPERAND                 
         BNE   DATAR21             FOR PUTS                                     
         CLI   RAVLNCAT,C'P'                                                    
         BNE   DATAR21                                                          
         OC    RAVLNOP2,RAVLNOP2                                                
         BZ    DATAR21                                                          
         MVI   ERROR,INVALID                                                    
         J     ERREND                                                           
         SPACE 1                                                                
DATAR21  CLI   RAVLNTYP,0          MANIPULATION OF UPGRADE DATA                 
         BE    DATAR22                                                          
         OC    RAVLNOP1,RAVLNOP1                                                
         BZ    DATAR22                                                          
         OC    RAVLNOP2,RAVLNOP2                                                
         BNZ   DATAR22                                                          
         MVC   RAVLNOP2,INVFBK                                                  
         DROP  RE                                                               
         SPACE                                                                  
DATAR22  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,(R4)),WORK,0                        
         SPACE                                                                  
         MVI   BYTE2,0                                                          
         CLI   INVTYP,C'I'         TEST FOR INV TO INV TRANSFER                 
         BNE   *+8                                                              
         MVI   BYTE2,C'I'                                                       
                                                                                
         MVI   BYTE,0              SET PRECISION TYPE FOR DEMO CALCS            
         CLI   TAPEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,C'I'                                                        
*                                  BECAUSE OF UT/TP PROBLEM                     
         NI    11(R4),X'FF'-X'40'                                               
         L     R7,ACOMFACS                                                      
         XC    MYWORK,MYWORK                                                    
*                                                                               
*!!!!    MVC   RIDBLK(4),=CL4'RID=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK(4),=CL4'RI2=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK+4(2),AGENCY                                               
         MVC   RIDBLK+6(1),TIMECHG  TIME CHANGE FLAG                            
*                                                                               
         GOTO1 DEMUP,DMCB,(BYTE2,34(R4)),(BYTE,WORK),(R7),RIDBLK                
         OI    11(R4),X'40'                                                     
         B     DATAR25                                                          
         SPACE 2                                                                
* CODE FOR MISSING UPGRADE INPUT.  FORCE UPGRADE TO GET HPT DATA                
* FOR TO DAY/TIME AND SOURCE/BOOK.                                              
*                                                                               
DATAR24  DS    0H                                                               
         CLC   INVCODE,=C'PJ'      CODE PJ REQUIRES UPGRADE                     
         BNE   DATAR24A                                                         
*                                                                               
*        CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
*        BE    DATAR25                                                          
*                                                                               
         MVC   RERROR,=AL2(MISINP)                                              
         J     ERREND2                                                          
*                                                                               
DATAR24A XC    WORK,WORK           BUILD FORCED UPGRADE ELEMENT                 
         XC    WORK2,WORK2                                                      
         MVC   WORK2+8(6),=C'IX,100'                                            
         MVI   WORK2+0,14                                                       
         MVI   WORK2+5,6                                                        
         GOTO1 UPVAL,DMCB,(1,WORK2),(C'Y',WORK),ACOMFACS                        
         CLI   DMCB,1                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,(R4)),WORK,0                        
         MVI   BYTE2,0                                                          
         CLI   INVTYP,C'I'         TEST FOR INV TO INV TRANSFER                 
         BNE   *+8                                                              
         MVI   BYTE2,C'I'                                                       
                                                                                
         MVI   BYTE,0              SET PRECISION TYPE FOR DEMO CALCS            
         CLI   TAPEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,C'I'                                                        
*                                  BECAUSE OF UT/TP PROBLEM                     
         NI    11(R4),X'FF'-X'40'                                               
         L     R7,ACOMFACS                                                      
         XC    MYWORK,MYWORK                                                    
*                                                                               
*!!!     MVC   RIDBLK(4),=CL4'RID=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK(4),=CL4'RI2=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK+4(2),AGENCY                                               
         MVC   RIDBLK+6(1),TIMECHG  TIME CHANGE FLAG                            
*                                                                               
         GOTO1 DEMUP,(R1),(BYTE2,34(R4)),(BYTE,WORK),(R7),RIDBLK                
         OI    11(R4),X'40'                                                     
         B     DATAR25                                                          
         SPACE                                                                  
DATAR25  EQU   *                                                                
         MVI   TRMODE,C'P'                                                      
         GOTO1 =A(BUFFER),DMCB,(RC),RR=YES                                      
         LA    R5,OUTLN(R5)                                                     
         BCT   R3,DATAR16                                                       
         DROP  R5                                                               
         SPACE                                                                  
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVI   TRMODE,C'F'         FINAL BUFFERING                              
         GOTO1 =A(BUFFER),DMCB,(RC),RR=YES                                      
         MVI   TRMODE,C'W'         HANDLE I/O TO REPFILE                        
         GOTO1 =A(BUFFER),DMCB,(RC),RR=YES                                      
*                                                                               
         L     R1,THISLINE                                                      
         MVC   SVSINV,0(R1)                                                     
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------            
         SPACE 2                                                                
CODETAB  DS    0CL3                                                             
         DC    C'TP',AL1(PRO+TP)                                                
         DC    C'TT',AL1(PRO+TP)                                                
         DC    C'ES',AL1(PRO+INV)                                               
         DC    C'PJ',AL1(PRO+INV)                                               
         DC    C'PR',AL1(PRO)                                                   
         DC    C'PA',AL1(PRO+INV)                                               
         DC    C'PT',AL1(PRO+INV+MIX)                                           
         DC    C'TE',AL1(PRO+INV)                                               
         DC    C'PE',AL1(PRO+INV)                                               
         DC    C'NT',AL1(PRO+INV)                                               
         DC    C'FT',AL1(PRO+INV)                                               
         DC    C'MT',AL1(PRO+INV)                                               
         DC    C'YT',AL1(PRO+INV)                                               
         DC    C'JT',AL1(PRO+INV)                                               
         DC    C'OT',AL1(PRO+INV)                                               
         DC    C'RT',AL1(PRO+INV)                                               
         DC    C'NP',AL1(PRO+INV)                                               
         DC    C'FP',AL1(PRO+INV)                                               
         DC    C'MP',AL1(PRO+INV)                                               
         DC    C'YP',AL1(PRO+INV)                                               
         DC    C'OP',AL1(PRO+INV)                                               
         DC    C'RP',AL1(PRO+INV)                                               
         DC    C'JP',AL1(PRO+INV)                                               
         DC    C'  ',AL1(PRO+INV)                                               
CODES    EQU   (*-CODETAB)/L'CODETAB                                            
         SPACE                                                                  
MONTAB   DC    C'NFMAYJO'                                                       
MONTHSS  EQU   (*-MONTAB)                                                       
*                                                                               
*  VARIABLE LENGTH MOVE ROUTINE                                                 
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
         SPACE 2                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*-------------------------------------------------------------------            
*                 GET BOOK TYPE IN EBCDIC                                       
*-------------------------------------------------------------------            
GETBKTYP NTR1  BASE=*,LABEL=*                                                   
         XC    BKTYPE,BKTYPE                                                    
*                                                                               
         TM    0(R6),X'20'         ESTIMATED BOOK?                              
         BZ    GETBK10                                                          
         MVI   BKTYPE,C'E'                                                      
         B     GETBKX                                                           
*                                                                               
GETBK10  TM    0(R6),X'04'         PROJECTED BOOK?                              
         BZ    GETBK20                                                          
         MVI   BKTYPE,C'P'                                                      
         B     GETBKX                                                           
*                                                                               
GETBK20  TM    0(R6),X'02'         SPECIAL BOOK?                                
         BZ    GETBK30                                                          
         MVI   BKTYPE,C'S'                                                      
         B     GETBKX                                                           
*                                                                               
GETBK30  TM    0(R6),X'08'         TIME PERIOD?                                 
         BZ    GETBKX                                                           
         MVI   BKTYPE,C'T'                                                      
         B     GETBKX                                                           
*                                                                               
GETBKX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*  SET DAY TIME TABLE FOR DEMO CALL                                             
*                                                                               
DYTIMSET NTR1  BASE=*,LABEL=*                                                   
         LA    R2,INVDYTIM                                                      
         USING DBXTLD,R2                                                        
         LA    R5,DYTIMTAB                                                      
*                                                                               
         XC    INVDYTIM,INVDYTIM                                                
         XC    DYTIMTAB,DYTIMTAB                                                
         MVC   DBXTLID,=CL4'DYTM'                                               
         LA    R3,DBXTLIST                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         B     DYTMS100                                                         
*                                                                               
DYTMS060 ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         CLI   0(RE),X'02'         DAY TIME ELEMENT                             
         BNE   DYTMSEX                                                          
         LA    R3,5(R3)                                                         
         LA    R5,5(R5)                                                         
*                                                                               
DYTMS100 MVC   0(1,R3),2(RE)       USE HEADER'S DAY                             
         MVC   1(4,R3),3(RE)       USE HEADER'S TIME                            
         MVC   0(5,R5),2(RE)       MOVE DAY/TIME INTO TABLE                     
         CLC   5(2,RE),=C'CC'      TEST FOR TO CONCLUSION                       
         BNE   DYTMS060            GET NEXT ELEMENT                             
         SR    R1,R1               ADD 2 HOURS TO START                         
         ICM   R1,3,1(R3)                                                       
         AHI   R1,200                                                           
         CHI   R1,2400             TEST FOR RUN PAST MIDNIGHT                   
         BNH   *+8                                                              
         SHI   R1,2400                                                          
         STCM  R1,3,1(R3)          SET END TIME                                 
         B     DYTMS060                                                         
*                                                                               
DYTMSEX  XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*  BUILD CE ELEMENTS FROM TABLE                                                 
*                                                                               
BLDCE    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'CE',AIO),0                          
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'CF',AIO),0                          
*                                                                               
         LA    R5,DYTIMTAB                                                      
*                                                                               
BLDCE050 CLI   0(R5),0                                                          
         BE    BLDCEEX                                                          
*                                                                               
         XC    WORK,WORK           PUT IN 'CE' EL BEFORE POSSIBLE               
         MVC   WORK(2),=X'CE0A'    DEMUP CALL.                                  
         MVC   WORK+2(5),0(R5)     HEADER DAY TIME                              
         MVC   WORK+7(3),INVSRC    FROM BOOK                                    
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),WORK,0                         
         LA    R5,5(R5)                                                         
         B     BLDCE050                                                         
*                                                                               
BLDCEEX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
BUFFER   NTR1  BASE=*,LABEL=*                                                   
         CLI   TRMODE,C'P'                                                      
         BE    BUFPUT                                                           
         CLI   TRMODE,C'I'                                                      
         BE    BUFINIT                                                          
         CLI   TRMODE,C'F'                                                      
         BE    BUFFIN                                                           
         CLI   TRMODE,C'W'                                                      
         BE    BUFWRT                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
BUFINIT  DS    0H                  INITIALIZE                                   
         XC    TRPAGE(2),TRPAGE    CLEAR PAGE AND RECORD COUNT                  
         MVC   TEMPLEN,=XL2'0FA0'                                               
         LA    RE,BUFF                                                          
         ST    RE,TRAPAGE          SET POINTER TO PAGE AREA FOR BUFFER          
         LA    RF,4000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR THE PAGE AREA                          
         B     BUFFERX                                                          
         SPACE                                                                  
*                                                                               
* PUT TWO RECORDS INTO BUFFER - WHEN TWO ARE THERE, WRITE A PAGE TO TWA         
*                                                                               
BUFPUT   DS    0H                                                               
         L     R5,TRAPAGE                                                       
         CLI   0(R5),0             FIRST I/O AREA FREE                          
         BE    BUFPUT2             YES                                          
         LA    R5,2000(R5)         NO-TRY NEXT ONE                              
         CLI   0(R5),0                                                          
         BE    BUFPUT2             SECOND IS FREE                               
*                                                                               
         L     R5,TRAPAGE          WRITE PAGE WITH RECORDS IN IT FIRST          
         ZIC   R6,TRPAGE                                                        
         LA    R6,1(R6)                                                         
         STC   R6,TRPAGE           UPDATE PAGES ALREADY WRITTEN                 
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R6,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TEMPLEN                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,(R5)                        
         LR    RE,R5                                                            
         LA    RF,4000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               RE-CLEAR PAGE AREA                           
*                                                                               
BUFPUT2  DS    0H                                                               
         GOTO1 MOVEREC,DMCB,AIO,(R5)                                            
         ZIC   R1,TRRECS                                                        
         LA    R1,1(R1)            INCREMENT COUNT OF RECORDS ALREADY           
         STC   R1,TRRECS           WRITTEN                                      
         B     BUFFERX                                                          
*                                                                               
* WRITE LAST PAGE OUT TO TWA                                                    
*                                                                               
BUFFIN   DS    0H                  END OF LINE ROUTINE                          
         L     R5,TRAPAGE                                                       
         ZIC   R6,TRPAGE                                                        
         LA    R6,1(R6)                                                         
         STC   R6,TRPAGE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R6,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)                                                 
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TEMPLEN                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,(R5)                        
         B     BUFFERX                                                          
*                                                                               
* READ IN PAGES, MOVE RECORDS TO REC, AND WRITE THEM TO REPFILE                 
*                                                                               
BUFWRT   DS    0H                                                               
         SR    R3,R3               PAGE COUNTER                                 
*                                                                               
BUFWRT2  DS    0H                                                               
         LA    R3,1(R3)                                                         
         L     R5,TRAPAGE                                                       
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R3,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)                                                 
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TEMPLEN                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R5)                        
         LA    R6,2                                                             
*                                                                               
BUFWRT4  DS    0H                                                               
         GOTO1 MOVEREC,DMCB,(R5),AIO                                            
         BAS   RE,FLADD                                                         
         LA    R5,2000(R5)                                                      
         BCT   R6,*+8                                                           
         B     BUFWRT6                                                          
         CLI   0(R5),0             SECOND RECORD ON PAGE                        
         BNE   BUFWRT4             YES                                          
*                                                                               
BUFWRT6  DS    0H                                                               
         CLM   R3,1,TRPAGE                                                      
         BL    BUFWRT2                                                          
*                                                                               
BUFFERX  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              ADD THE RECORD TO FILE                                           
         SPACE 1                                                                
FLADD    NTR1  BASE=*,LABEL=*                                                   
         USING REINVREC,R4                                                      
         USING RINVAEL,R5                                                       
         MVC   KEY,RINVREC                                                      
         SPACE 1                                                                
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=X'EF0C'                                             
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVAFST)                                 
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         MVI   RINVAWHY,C'A'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    FLPUT                                                            
* !!!!                                                                          
         OI    MYFLAG,DOTRANS                                                   
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R5)                           
         BRAS  RE,MYFILADD         ADD THE RECORD                               
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETES                        
         B     FLX                                                              
         SPACE 1                                                                
FLPUT    TM    KEY+27,X'80'                                                     
         BNO   *+12                                                             
         MVI   KEY+27,0                                                         
         BRAS  RE,MYDIRWRT         UNDELETE THE POINTER                         
         L     RE,AIO3                                                          
         ST    RE,AIO                                                           
         GOTO1 GETREC              GET OLD RECORD IN REC2                       
         SPACE 1                                                                
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'EF',AIO),0                          
         L     R5,12(R1)                                                        
         CLI   12(R1),0                                                         
         BNE   FLP10               NOT THERE - BUILD IT FROM SCRATCH            
* !!!!                                                                          
         MVC   SVALST,RINVALST                                                  
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         CLC   SVALST,RINVALST     LAST ACTIVITY DATE = TODAY'S DATE            
         BE    *+8                                                              
         OI    MYFLAG,DOTRANS                                                   
         B     FLP15                                                            
*                                                                               
FLP10    DS    0H                                                               
         LA    R5,WORK                                                          
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         OI    MYFLAG,DOTRANS                                                   
*                                                                               
FLP15    MVI   RINVAWHY,C'C'                                                    
         SPACE 1                                                                
         L     RE,AIO2                                                          
         ST    RE,AIO                                                           
         SPACE 1                                                                
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'EF',AIO),0                          
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R5)                           
         BRAS  RE,MYFILWRT         WRITE BACK THE NEW                           
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
FLX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*              PARAMETER 1 =       A(FROM RECORD AREA)                          
*              PARAMETER 2 =       A(TO RECORD AREA)                            
*                                                                               
MOVEREC  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            FROM REC                                     
         L     R3,4(R1)            TO REC                                       
*                                                                               
         MVC   HALF,27(R2)         FROM REC LEN                                 
         LH    R5,HALF                                                          
         LA    R4,0(R5,R2)                                                      
         MVI   0(R4),0             FOR RECORDS ADDED BY HELLO                   
MOVE100  LTR   R5,R5                                                            
         BZ    MOVEXIT                                                          
*                                                                               
         CHI   R5,250                                                           
         BNH   MOVEREST                                                         
         MVC   0(250,R3),0(R2)                                                  
         LA    R2,250(R2)                                                       
         LA    R3,250(R3)                                                       
         SHI   R5,250                                                           
         B     MOVE100                                                          
MOVEREST BCTR  R5,R0                                                            
         EX    R5,MOVEVAR                                                       
MOVEXIT  L     R6,4(R1)                                                         
         LH    R5,27(R6)                                                        
         AR    R6,R5                                                            
         MVI   0(R6),0                                                          
         XIT1                                                                   
MOVEVAR  MVC   0(0,R3),0(R2)                                                    
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
INVLST   NTR1  BASE=*,LABEL=*                                                   
         MVI   INVNO,0                                                          
         MVI   ERROR,2             PRESET ERROR MESSAGE NUMBER                  
         L     R4,INVLIST          OUTPUT AREA                                  
         USING INVLD,R4                                                         
         XC    INVLREC,INVLREC                                                  
         MVC   INVLFLE,INVTYP                                                   
         SPACE 1                                                                
         LM    R2,R3,0(R1)         A(FIELD HEADER),  A(WORK AREA)               
         XR    R5,R5                                                            
         SPACE 1                                                                
         USING COMFACSD,R7                                                      
         L     R7,ACOMFACS                                                      
         GOTO1 CSCANNER,DMCB,(R2),((R5),(R3))                                   
         CLI   DMCB+4,0                                                         
         BE    INVERR                                                           
         SPACE 1                                                                
         XR    R7,R7                                                            
         IC    R7,DMCB+4           NUMBER OF INPUT FIELDS                       
         SPACE 1                                                                
PREFIX   OI    INVLTYP,X'08'                                                    
         CLI   12(R3),C'+'         ADD EXPRESSION                               
         BE    PREFIX2                                                          
         NI    INVLTYP,X'FF'-X'08' TURN OFF ADD BIT                             
         CLI   12(R3),C'/'         FOOTNOTE SUPPRESSION                         
         BNE   STATION                                                          
         MVI   TRFNOVER,C'Y'       YES-SET A SWITCH                             
         SPACE 1                                                                
PREFIX2  XR    R6,R6               REMOVE PREFIX CHARACTER                      
         IC    R6,0(R3)            AND DECREMENT BLOCK LENGTH                   
         BCTR  R6,0                                                             
         STC   R6,0(R3)                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),13(R3)     SHIFT LEFT                                   
         LA    R6,13(R6,R3)                                                     
         MVI   0(R6),C' '          CLEAR LAST                                   
         SPACE 1                                                                
STATION  XR    R5,R5                                                            
         IC    R5,0(R3)            LENGTH OF DAY EXPRESSION                     
         CLI   12(R3),C'#'         NHTI PREFIX# = HEX PRG NUMBER                
         BE    NHTNUM                                                           
                                                                                
                                                                                
*     CHECK IF 1ST ITEM A NON-STANDARD ROTATION REQUEST                         
                                                                                
         GOTOR CHKNSR,DMCB,((R5),12(R3)),DUB                                    
         CLI   DUB,0                                                            
         BNE   DAYTIM20            FIRST IS A NON-STANDARD ROTATION             
*                                                                               
                                                                                
STAT10   GOTO1 VINVDAY,DMCB,((R5),12(R3)),DUB,DUB+1,DAYVAL                      
         CLI   DUB,0                                                            
         BNE   DAYTIM20            FIRST IS A DAY                               
         SPACE 1                                                                
         GOTO1 VPAVSTA,DMCB,12(R3),DMCB+8    IS FIRST A STATION                 
         CLI   DMCB+4,X'FF'                                                     
         BE    NUM                 NOT A STATION                                
         MVC   DEMSTA,DMCB+8            SAVE STATION FOR GET DEMOS              
         MVC   DEMEDIA,DMCB+13           MEDIA                                  
         CHI   R7,2                                                             
         BL    INVERR                                                           
         B     INVSKIP                                                          
         EJECT                                                                  
*                                                                               
NHTNUM   DS    0H                                                               
         CLI   0(R3),5                                                          
         BNE   NHTNUMNX                                                         
         LA    R1,13(R3)                                                        
         B     *+16                                                             
NHTNUM5  CLI   0(R3),4                                                          
         BNE   NHTNUMNX                                                         
         LA    R1,12(R3)                                                        
*                                                                               
         ST    R1,DMCB                                                          
         XR    R5,R5                                                            
         IC    R5,0(R3)                                                         
         MVC   INVLFLE,INVTYP                                                   
         L     RF,ACOMFACS                                                      
         L     RF,(CHEXIN-COMFACSD)(RF)                                         
         GOTO1 (RF),DMCB,,INVLNUMB,4                                            
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   NUM6                GO SET INDICATORS                            
NHTNUMNX MVI   ERROR,INVALID                                                    
         B     INVERR              INVALID PRG NUMBER                           
                                                                                
*              EDIT INVENTORY NUMBER                                            
*                                                                               
NUM      XR    R5,R5                                                            
         MVC   INVLFLE,INVTYP                                                   
*  ARE WE DEALING WITH SELF DEFINED INVENTORY                                   
         TM    RMPPROFS,X'80'                                                   
         BNO   NUM1                                                             
         CLI   12(R3),C'*'                                                      
         BNE   NUM1                                                             
         MVC   12(4,R3),13(R3)                                                  
         B     NUM2                                                             
*                                                                               
NUM1     IC    R5,0(R3)                                                         
         CHI   R5,4                INVENTORY NUMBER MUST BE 3 OR 4              
         BH    DAYTIM                                                           
         CHI   R5,3                                                             
         BL    DAYTIM                                                           
         SPACE 1                                                                
         CLI   12(R3),C'0'         QTR  HOUR NUMBER MUST BE 00- 99              
         BL    DAYTIM                                                           
         CLI   12(R3),C'9'                                                      
         BH    DAYTIM                                                           
         CLI   13(R3),C'0'                                                      
         BL    DAYTIM                                                           
         CLI   13(R3),C'9'                                                      
         BH    DAYTIM                                                           
         SPACE 1                                                                
NUM2     CLI   INVTYP,C'I'                                                      
         BNE   *+20                                                             
         MVC   INVLNUMB,12(R3)     MOVE INVENTORY NUMBER OUT                    
         OC    INVLNUMB(4),=4X'40'                                              
         B     NUM6                                                             
*                                                                               
         CLI   DEMSTA+4,C'H'                                                    
         BE    INVERR            PURE NUMBER IS INVALID FOR NETWORK             
         PACK  DUB(8),12(2,R3)                                                  
         CVB   R0,DUB                                                           
         STC   R0,INVLNUMB            QTR HOUR NUMBER                           
         CLI   14(R3),C'D'         TYPICAL                                      
         BE    NUM3                                                             
         CLI   14(R3),C'E'         WEEKEND                                      
         BE    NUM3                                                             
         CLI   14(R3),C'0'                                                      
         BL    INVERR                                                           
         CLI   14(R3),C'9'                                                      
         BH    INVERR                                                           
         SPACE 1                                                                
NUM3     MVC   INVLNUMB+1(1),14(R3)      DAY CODE                               
         CLI   INVLFLE,C'I'                                                     
         BE    NUM4                                                             
         GOTO1 VINVDAY,DMCB,1,(0,14(R3)),(0,INVLNUMB+1)  PAV DAY CODE           
         SPACE 1                                                                
         CHI   R5,3                PURE  3 CHARACTER INPUT                      
         BE    NUM6               IS OK                                         
         SR    RF,RF                                                            
         IC    RF,15(R3)           START WEEK                                   
         XR    RE,RE                                                            
         SLDL  RE,28               RE 4 HIGH ORDER                              
         SRL   RF,28               RF 4 LOW ORDER                               
         CHI   RF,7                                                             
         BH    INVERR              0-7 , A-G                                    
         SLL   RF,1                                                             
         EX    RF,*+8              BITES 4-6                                    
         B     *+8                                                              
         OI    INVLNUMB+1,0                                                     
         SPACE 1                                                                
         CHI   RE,15                                                            
         BNE   *+14                NOT NUMERIC                                  
         LTR   RF,RF                                                            
         BZ    *+16                INPUT ZERO                                   
         B     NUM6                                                             
         SPACE 1                                                                
         CHI   RE,12                                                            
         BNE   INVERR              NOT A-G                                      
         OI    INVLNUMB+1,1        LOW ORDER ON FOR ZERO AND A-G                
         B     NUM6                                                             
         SPACE 1                                                                
NUM4     MVI   INVLNUMB+2,C'0'      DEFAULT LENGTH                              
         CHI   R5,3                                                             
         BE    *+10                                                             
         MVC   INVLNUMB+2(1),15(R3) LENGTH OR SPECIAL INPUT CODE                
         SPACE 1                                                                
NUM6     OI    INVLTYP,X'80'         INPUT WAS AN INVENTORY NUMBER              
         MVI   INVLWT,0             WEIGHT                                      
         L     RE,THISLINE                                                      
         LA    RE,FROM(RE)                                                      
         CLI   0(RE),C'+'                                                       
         BNE   *+8                                                              
         MVI   INVLWT,1              IF ADD SET WEIGHT TO 1                     
         SPACE 1                                                                
         CLI   1(R3),0             NO WEIGHT                                    
         BE    DATE                                                             
         OC    8(4,R3),8(R3)                                                    
         BZ    INVERR              WEIGHT NOT NUMERIC                           
         CLC   8(4,R3),=F'99'                                                   
         BH    INVERR              99 SHOULD BE ENOUGH                          
         MVC   INVLWT,11(R3)      WEIGHT TO LIST                                
         OI    INVLTYP,X'04'       USER WEIGHTING OVERRIDE                      
         SPACE 1                                                                
DATE     CLI   INVLFLE,C'P'                                                     
         BE    INVNEXT                                                          
         CHI   R7,2                DO I HAVE ONE LEFT                           
         BL    INVNEXT             NO, SO GET OUT                               
         LA    R6,32(R3)           NEXT SCAN AREA                               
*                                                                               
DATE1    GOTO1 DATVAL,DMCB,(0,12(R6)),DUB                                       
         OC    DMCB,DMCB                                                        
         BZ    INVNEXT                                                          
         CLI   1(R6),0             MUST BE A DATE ENTRY                         
         BE    *+12                                                             
         MVI   ERROR,INVALID       INVALID ENTRY FORMULA                        
         B     INVERR                                                           
         SPACE 1                                                                
         LA    R3,32(R3)                                                        
         BCTR  R7,0                                                             
         GOTO1 DATCON,DMCB,(0,DUB),(3,INVLDATE)                                 
         B     INVNEXT                                                          
         EJECT                                                                  
*              EDIT A DAY/TIME EXPRESSION                                       
*                                                                               
DAYTIM   XR    R5,R5                                                            
         IC    R5,0(R3)            LENGTH OF DAY EXPRESSION                     
                                                                                
*        1ST, CHECK IF A NONSTANDARD ROTATION REQUEST                           
                                                                                
         GOTOR CHKNSR,DMCB,((R5),12(R3)),DUB                                    
         CLI   DUB,0                                                            
         BNE   DAYTIM20                                                         
                                                                                
         GOTO1 VINVDAY,DMCB,((R5),12(R3)),DUB,DUB+1,DAYVAL                      
         CLI   DUB,0                                                            
         BE    INVERR              NOT A VALID DAY                              
                                                                                
DAYTIM20 CLI   1(R3),0                                                          
         BNE   INVERR              DAY= IS INVALID                              
         SPACE 1                                                                
         CLI   INVLFLE,C'I'                                                     
         BE    DAYTIM60                                                         
                                                                                
         GOTOR CHKNSR,DMCB,((R5),12(R3)),INVLDAY                                
         CLI   INVLDAY,0                                                        
         BNE   DAYTIM30                                                         
                                                                                
         GOTO1 DAYVAL,DMCB,((R5),12(R3)),INVLDAY,DUB                            
DAYTIM30 OI    INVLTYP,X'60'                                                    
         SPACE 1                                                                
DAYTIM40 BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BZ    INVERR                                                           
         LA    R3,32(R3)      CHECK FOR MULTIPLE DAYS                           
         IC    R5,0(R3)                                                         
                                                                                
DAYTIM50 GOTOR CHKNSR,DMCB,((R5),12(R3)),DUB                                    
         CLI   DUB,0                                                            
         BNE   DAYTIM55                                                         
                                                                                
         GOTO1 DAYVAL,DMCB,((R5),12(R3)),DUB,DUB+1                              
         CLI   DUB,0          NOT A DAY                                         
         BE    DAYTIM80                                                         
DAYTIM55 OC    INVLDAY,DUB                                                      
         B     DAYTIM40                                                         
         SPACE 1                                                                
DAYTIM60 BCTR  R7,0                NOW I EXPECT TO FIND A TIME FIELD            
         LTR   R7,R7                                                            
         BZ    INVERR              BUT THERE IS NO MORE INPUT                   
         LA    R3,32(R3)           R3 TO TIME FIELD                             
         XR    R5,R5                                                            
         IC    R5,0(R3)            LENGTH OF THE TIME EXPRESSION                
         SPACE 1                                                                
DAYTIM80 GOTO1 TIMVAL,DMCB,((R5),12(R3)),DUB+4                                  
         CLI   DMCB,X'FF'                                                       
         BE    INVERR              INVALID TIME                                 
         SPACE 1                                                                
         CLC   DUB+6(2),=H'2400'                                                
         BH    INVERR              DO NOT ACCEPT -CC, NONE, OR VARY             
         SPACE 1                                                                
         CLI   INVLFLE,C'P'                                                     
         BNE   *+14                                                             
         MVC   INVLSTIM(4),DUB+4       START/END TIME FOR PURE                  
         B     INVNEXT                                                          
         SPACE 1                                                                
         GOTO1 VHRTOQH,DMCB,DUB+4,FULL                                          
         XR    R5,R5                                                            
         IC    R5,FULL           START QTR HR CODE                              
         STC   R5,INVLNUMB            QTR HOUR CODE                             
         MVC   INVLNUMB+1(1),DUB        INV DAY CODE                            
         MVI   INVLNUMB+2,C'0'          LENGTH                                  
         SPACE 1                                                                
         MVI   INVLWT,1             WEIGHT                                      
         OI    INVLTYP,X'60'         FIRST AND LAST                             
         SPACE 1                                                                
         OC    DUB+6(2),DUB+6                                                   
         BZ    INVNEXT             NO END TIME                                  
         SPACE 1                                                                
         GOTO1 VHRTOQH,DMCB,DUB+6,FULL  END QTR HOUR                            
         XR    RE,RE                                                            
         IC    RE,FULL           END QTR CODE                                   
         SR    RE,R5               END MINUS START                              
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BNP   INVNEXT             THIS SHOULD NEVER HAPPEN                     
         NI    INVLTYP,X'4F'         TURN OFF LAST                              
         SPACE 1                                                                
DAYTIM90 MVC   10(10,R4),0(R4)       NEXT OUTPUT AREA                           
         LA    R4,10(R4)                                                        
         SPACE 1                                                                
         AHI   R5,1               NEXT QTR HOUR                                 
         STC   R5,INVLNUMB                                                      
         NI    INVLTYP,X'0F'                                                    
         BCT   RE,DAYTIM90                                                      
         OI    INVLTYP,X'20'         LAST IN LIST                               
         EJECT                                                                  
INVNEXT  LA    R4,10(R4)            NEXT OUTPUT AREA                            
         XC    0(10,R4),0(R4)                                                   
         SPACE 1                                                                
INVSKIP  LA    R3,32(R3)           NEXT SCAN AREA                               
         LA    RF,NUM                                                           
         CLI   DEMSTA+4,C'H'       NHTI STATION?                                
         BNE   *+8                                                              
         LA    RF,NHTNUM5                                                       
         BCTR  R7,RF                                                            
         SPACE 1                                                                
         L     R4,INVLIST            OUTPUT AREA                                
         XR    R5,R5                                                            
         OC    0(10,R4),0(R4)       LOOK FOR END                                
         BZ    *+16                                                             
         LA    R5,1(R5)                                                         
         LA    R4,10(R4)                                                        
         B     *-18                                                             
         SPACE 1                                                                
         STC   R5,INVNO            RETURN NUMBER OF ITEMS                       
         XIT1                                                                   
*                                                                               
INVERR   L     R2,THISLINE                                                      
         LA    R2,FROMH(R2)                                                     
         J     ERREND                                                           
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
****************************************************************                
*                                                              *                
*        CHECK FOR NON-STANDARD ROTATION REQUEST EXPRESSION    *                
*              P1=ADDR OF EXPRESSION,P2=ADDR FOR OUTPUT BYTE   *                
*                                                              *                
****************************************************************                
*                                                                               
CHKNSR   NTR1  BASE=*,LABEL=*                                                   
*                                  VALIDATE FOR "AVN" OR "AVGN"                 
                                                                                
         ZICM  R2,1(R1),3          R2 POINTS AT EXPRESSION TO CHECK             
         ZIC   RF,0(R1)            RF HAS EXPRESSION LENGTH                     
         L     R3,4(R1)            ADDR FOR 1 BYTE OUTPUT                       
*                                                                               
CHKNS10  DS    0H                  VALIDATE FOR "VAR"                           
         CHI   RF,3                MUST BE EXACTLY 3 LONG TO BE VAR             
         BNE   CHKNS50             NOPE, CHECK IF AVGN EXPRESSION               
         CLC   0(3,R2),=C'VAR'     ELSE, CHECK FOR MATCH                        
         BNE   CHKNS50              NO,  CHECK IF AVGN EXPRESSION               
         CLI   INVTYP,C'P'          YES,IF NOT A PAV REQUEST                    
         BNE   CHKNSERR                    THEN ERROR                           
         MVI   0(R3),X'90'                                                      
         B     CHKNSX                                                           
                                                                                
CHKNS50  SHI   RF,2                SUB 1 FOR END DIGIT+1 FOR EX INSTR           
         BNP   CHKNSNO             TOO SHORT TO BE VALID NSR REQUEST            
         EXCLC RF,0(R2),=C'AVG'     LOOK FOR "AV" OR "AVG"                      
         BNE   CHKNSNO               NOPE                                       
         CLI   INVTYP,C'P'          FOUND, IF NOT A PAV REQUEST                 
         BNE   CHKNSERR                     ERROR                               
*                                          ELSE, CONTINUE                       
         LA    RE,1(RF,R2)         RE-->LAST CHAR OF INPUT                      
         CLI   0(RE),C'2'           SHOULD BE BETWEEN 2                         
         BL    CHKNSNO                                                          
         CLI   0(RE),C'6'           AND 6 DAYS                                  
         BH    CHKNSNO                                                          
         MVC   0(1,R3),0(RE)                                                    
         NI    0(R3),X'9F'          X'90' BITS ==> VAR OR AVG-N                 
         B     CHKNSX                                                           
                                                                                
CHKNSNO  MVI   0(R3),0                                                          
*                                                                               
CHKNSX   XIT1                                                                   
*                                                                               
CHKNSERR L     R2,THISLINE                                                      
         LA    R2,FROMH(R2)                                                     
         J     ERREND                                                           
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
****************************************************************                
****************************************************************                
*              LIST RECORD ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
LIST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    KEY(17),KEY                                                      
         BNZ   LR100                                                            
*                                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   SAVEKEY,KEY                                                      
LR100    GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 SEQ                                                              
LR220    LA    R6,KEY                                                           
         CLC   SAVEKEY(17),KEY                                                  
         BNE   LREXT                                                            
         CLI   RINVKSRC,X'00'      INVENTORY HEADER                             
         BNE   LR200                                                            
         MVC   SAVEKEY,KEY                                                      
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*--DATE FILTER                                                                  
         CLI   DTEHLD2,0                                                        
         BE    LR260                                                            
         CLI   RINVPEFF+2,0        DATE RANGE                                   
         BE    LR240               NO SINGLE DATE                               
         CLC   DTEHLD2,RINVPEFF                                                 
         BL    LR200                                                            
         CLC   DTEHLD2,RINVPEFF+2                                               
         BH    LR200                                                            
         B     LR260                                                            
*--SINGLE DATE EDIT                                                             
LR240    CLC   DTEHLD2,RINVPEFF                                                 
         BH    LR200                                                            
*                                                                               
LR260    LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         LA    R2,MTRINVH          INVENTORY                                    
         MVC   LINVNUM(4),RINVKINV                                              
*                                                                               
         LA    R3,RINVPEFF         EFFECTIVE DATES                              
         LA    R4,LEFFDTE                                                       
         GOTO1 DATCON,DMCB,(2,0(R3)),(5,0(R4))                                  
         CLI   2(R3),0                                                          
         BE    LR280                                                            
         MVI   8(R4),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R3)),(5,9(R4))                                  
*                                                                               
LR280    MVC   LDPT,RINVDP         DAYPART                                      
*                                                                               
*  DAY/TIME                                                                     
         USING RIDTELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',(R6)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
*                                                                               
         GOTO1 UNDAY,DMCB,RIDTDAY,LDYTIME           DAY                         
         LA    RE,17                                                            
         LA    R3,LDYTIME                                                       
*                                                                               
LR320    CLI   0(R3),X'40'                                                      
         BNH   LR340                                                            
         LA    R3,1(R3)                                                         
         BCT   RE,LR320                                                         
         DC    H'0'                                                             
LR340    MVI   0(R3),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R3))       TIME                        
         DROP  R4                                                               
*                                                                               
*  PROGRAM                                                                      
         USING RIPGELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',(R6)),0                         
         CLI   12(R1),0                                                         
         BNE   LR500                                                            
         L     R4,12(R1)                                                        
         ZIC   R1,RIPGLEN                                                       
         S     R1,=F'2'                                                         
         C     R1,=F'20'           MAX OUTPUT SIZE                              
         BNH   *+8                                                              
         LA    R1,20                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     LR500                                                            
         MVC   LPROGRM(0),RIPGNAME                                              
*                                                                               
         SPACE                                                                  
LR500    GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         XIT1                                                                   
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
*                                                                               
***********************************************************************         
* ISSUE LTRANS REQUEST?                                                         
***********************************************************************         
GOLTRANS NTR1  BASE=*,LABEL=*                                                   
         TM    MYFLAG,DOTRANS      ISSUE LTRANS REQUEST?                        
         BZ    GOLTRANX            NO                                           
*                                                                               
         CLI   CSTAT+4,C'T'        TELEVISION?                                  
         BNE   *+8                                                              
         MVI   CSTAT+4,C' '        MOVE IN SPACE FOR LTRANS                     
*                                                                               
         NI    DMINBTS,X'FF'-X'80' TURN OFF DELETED REC READ                    
         GOTO1 VLTRANS             YES- ISSUE LTRANS REQUEST                    
         NI    MYFLAG,X'FF'-DOTRANS                                             
*                                                                               
         CLI   CSTAT+4,C' '        TELEVISION?                                  
         BNE   *+8                                                              
         MVI   CSTAT+4,C'T'        MOVE BACK 'T' FOR TELEVISION                 
*                                                                               
GOLTRANX XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,KEYCHECK                                                      
         GOTO1 DATAMGR,DMCB,=C'ADDREC',REPFILE,KEY+28,AIO,DMWORK                
         BRAS  RE,DMCHECK                                                       
         J     YESS                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
MYFILWRT NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,KEYCHECK                                                      
         GOTO1 DATAMGR,DMCB,=C'PUTREC',REPFILE,KEY+28,AIO,DMWORK                
         BRAS  RE,DMCHECK                                                       
         MVC   BSVDA,KEY+28                                                     
         J     YESS                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
MYDIRWRT NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',REPDIR,KEY,KEY                           
         BRAS  RE,DMCHECK                                                       
         J     YESS                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*YDIRADD NTR1                                                                   
*        GOTO1 DATAMGR,DMCB,=C'DMADD',REPDIR,KEY,KEY                            
*        BRAS  RE,DMCHECK                                                       
*        J     YESS                                                             
*                                                                               
KEYCHECK DS    0H                                                               
         L     RF,AIO                                                           
         CLC   KEYSAVE(27),0(RF)                                                
         BER   RE                                                               
         DC    H'00'                                                            
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         JM    NOO                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
YESS     SR    R1,R1                                                            
         J     *+8                                                              
NOO      LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
ERREND   GOTO1 ERREX                                                            
ERREND2  GOTO1 MYERROR                                                          
*                                                                               
NASTN    EQU   350                 INVALID FOR STATION                          
*                                                                               
         EJECT                                                                  
         PRINT GEN                                                              
PLINED   DSECT                                                                  
PRINVNUM DS    CL4                                                              
         DS    CL2                                                              
PREFFDTE DS    CL17                                                             
         DS    CL2                                                              
PRPRGNM  DS    CL27                                                             
         DS    CL2                                                              
PRAVPROG DS    CL24                                                             
         DS    CL2                                                              
PRDYTIME DS    CL20                                                             
         DS    CL2                                                              
PRDAYPT  DS    CL7                                                              
         DS    CL2                                                              
PRAVDYTM DS    CL20                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
LINVNUM  DS    CL6                                                              
         DS    CL1                                                              
LEFFDTE  DS    CL17                                                             
         DS    CL1                                                              
LPROGRM  DS    CL20                                                             
         DS    CL1                                                              
LDPT     DS    CL6                                                              
         DS    CL1                                                              
LDYTIME  DS    CL18                                                             
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPPROF                                                                     
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPPROF                                                      
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPD9D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPD8D                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*  ALL FIELDS DEFINED ABOVE THE DOUBLE LINE OF ASTERIKS                         
*  MUST ALSO BE DEFINED IN THE RERMP30 PHASE.                                   
*                                                                               
INVLIST  DS    F                   POINTER TO INVENTORY INFO                    
INVDYTIM DS    CL60                EXTENDED DAY TIME DEMO TABLE                 
*                                                                               
INVMED   DS    CL1                 MEDIA                                        
INVSTAT  DS    CL5                 STATION                                      
INVMKT   DS    CL2                 MARKET                                       
INVSRC   DS    CL1                 SOURCE                                       
INVFBK   DS    CL2                 FROM BOOK                                    
INVTYP   DS    CL1                 I OR P                                       
INVEFF   DS    CL2                 EFFECTIVE DATE - COMPRESSED                  
INVNO    DS    CL1                 NUMBER IN INVENTORY LIST                     
INVBAD   DS    CL1                 0=NO ERROR, N=NUMBER OF BAD ITEM             
TOTWGHT  DS    CL2                 TOTAL NUMBER QTR HOURS                       
INVTOBK  DS    CL20                TO BOOK CODES (UP TO 5-4BYTE CODES)          
*                                                                               
INVIND   DS    CL1                 INVENTORY TYPE INDICATOR                     
INVDAYS  DS    CL1                 1=MON, 7=SUN                                 
INVTIM   DS    CL4                 MILITARY TIME                                
INVCODE  DS    CL2                 PROGRAM CODE                                 
INVCDCTL DS    B                   CONTROL BITS FOR PROGRAM CODE                
INVBTYPE DS    C                   BOOK TYPE (USER INPUT, APPLIES TO            
*                                  DEMO FILE TRANSFERS)                         
INVFRBT  DS    C                   BOOK TYPE (ON INV TO INV TRANSFER            
*                                                                               
TRBKLIST DS    CL64                BOOK ENTRIES BUILT BY REBKLST                
         SPACE                                                                  
TRBKCNT  DS    X                   COUNT OF BOOK ENTRIES                        
TRMODE   DS    C                   COMMUNICATION TO BUFFER ROUTINE              
TRWTOV   DS    C                   USER WEIGHTING OVERRIDE (Y/N)                
TRHOOKSW DS    C                   HOOK ENTERED FOR DEMAND CALL (Y/N)           
TRSVKEY  DS    CL27                                                             
TRFNOVER DS    C                   Y=SUPPRESS TIME PERIOD FOOTNOTING            
TRAPAGE  DS    A                   A(2304 BYTE PAGE)                            
TRPAGE   DS    X                   PAGES WRITTEN TO TWA                         
TRRECS   DS    X                   RECORDS GENERATED DURING LINE EDIT           
         SPACE 1                                                                
DEMEDIA  DS    CL1                 FROM MEDIA                                   
DEMSTA   DS    CL5                      STATION                                 
DEMRKT   DS    CL2                      MARKET FOR DEMOS                        
*                                                                               
HALF2    DS    H                                                                
BYTE2    DS    CL1                                                              
BYTE3    DS    CL1                                                              
BYTE4    DS    CL1                                                              
*                                                                               
TAPEOPT  DS    CL1                 Y ==> TAPE PRECISION FOR DEMO CALCS          
RIDBLK   DS    CL8                 DEMUP 4TH PARAMETER                          
*                                                                               
ACMMNCTE DS    A                   A(LINK TO "TALK" TO OTHER PHASES)            
*!!!                                                                            
TIMECHG  DS    CL1                 TIME CHANGE (S=SPRING F=FALL)                
WGTWEEK  DS    CL1                 INCLUDE WEEKS IN WEIGHT FACTOR               
*                                                                               
INVPRG#  DS    XL3                 PROGRAM# FOR SPECIFIC LOOKUP                 
         DS    XL28                (SPARE)                                      
*****************************************************                           
*****************************************************                           
*                                                                               
*              WORK AREA                                                        
STAHLD   DS    CL5                 STATION HOLD AREA                            
INVHLD   DS    CL4                 INVENTORY HOLD AREA                          
DTEHLD   DS    CL3                 DATE HOLD AREA                               
DTEHLD2  DS    CL2                 2 BYTE DATE HOLD AREA                        
DTEHLDE2 DS    CL2                 2 BYTE END DATE HOLD                         
IBLK     DS    CL5                 INPUT BLOCK FOR KSRC RTN                     
OBLK     DS    CL5                 OUTPUT BLK FOR KSRC ROUTINE                  
TEMPLEN  DS    CL2                 LENGTH OF TEMPSTR PAGE                       
*                                                                               
REPFILE  DS    CL8                 HELLO FILE NAME(MUST INITIALIZE)             
REPDIR   DS    CL8                 HELLO FILE NAME(MUST INITIALIZE)             
*                                                                               
*  PRINT ELEMENT ADDRESS STORAGE LOCATIONS                                      
DYTMPTR  DS    F                   DAY/TIME ELEMENT                             
PROGPTR  DS    F                   PROGRAM ELEMENT                              
AVPRPTR  DS    F                   AVAIL PROGRAM ELEMENT                        
OVFLSW   DS    CL1                 TOO MANY LINES TO PRINT                      
*                                                                               
BKTYPE   DS    CL1                 BOOK TYPE IN EBCDIC                          
TODAYBIN DS    XL3                 TODAY'S DATE IN BINARY                       
SVSINV   DS    CL104               SAVED SINV LINE                              
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
DOTRANS  EQU   X'01'               ISSUE LTRANS REQUEST                         
TNSDFTP  EQU   X'40'               TRANSFER DEFAULT = TP                        
TNSDFTT  EQU   X'20'               TRANSFER DEFAULT = TT                        
*                                                                               
SVALST   DS    CL3                 DATE OF LAST ACTIVITY (Y/M/D BIN)            
*                                                                               
WORK2    DS    CL200               EXTRA WORK AREA                              
THISLINE DS    A                   CURRENT LINE ADDRESS                         
SAVEKEY  DS    CL27                                                             
LTRANKEY DS    CL27                                                             
MYWORK   DS    CL8                 DEMUP 4TH PARAM.                             
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    F                   SAVED DISK ADDRESS                           
DYTIMTAB DS    CL41                DAY TIME TABLE                               
         SPACE 1                                                                
ATRANS   DS    A                                                                
VT81030  DS    A                   ADDRESS OF TRANSFER MODULE                   
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
       EJECT                                                                    
         SPACE 2                                                                
*                                                                               
*                                                                               
INDEX    DC    C'&&',X'FF'         INDEX UPGRADE MARKER                         
OFORMAT  DC    C'IUNUIUN',X'530B00'                                             
*                                                                               
DEMOSHR  DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
SHARES   DC    X'00',C'S',AL1(1)                                                
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
*        EQUATES                                                                
*                                                                               
MISINP   EQU   1                                                                
PRO      EQU   X'01'                                                            
INV      EQU   X'02'                                                            
TP       EQU   X'04'               READ TIME PERIOD FILE                        
MIX      EQU   X'08'               READ FROM PAV AND TIME PERIOD                
COMMA    EQU   C','                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
TYPEH    EQU   MTRTYP1H-MTRBOK1H                                                
TYPE     EQU   MTRTYP1-MTRBOK1H                                                 
CODEH    EQU   MTRCOD1H-MTRBOK1H                                                
CODE     EQU   MTRCOD1-MTRBOK1H                                                 
FROMH    EQU   MTRDET1H-MTRBOK1H                                                
FROM     EQU   MTRDET1-MTRBOK1H                                                 
UPGH     EQU   MTRUPG1H-MTRBOK1H                                                
UPG      EQU   MTRUPG1-MTRBOK1H                                                 
LINELEN  EQU   MTRBOK2H-MTRBOK1H                                                
         SPACE 2                                                                
         EJECT                                                                  
* DSECT TO COVER DEMO INTERFACE MODULE STORAGE                                  
*                                                                               
DEMOD    DSECT                                                                  
DIVISOR  DS    F                   DIVISOR BUCKET                               
ADATAREC DS    A                   A(DATA RECORD)                               
AINTEREC DS    A                   POINTER TO INTERIM RECORD (D/T)              
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
TOTSHR   DS    3F                  SHARE ACCUMULATORS                           
HOMSHR   DS    3F                                                               
         DS    0F                                                               
MATHFAC  DS    0CL17                                                            
MTHCFACS DS    A                   A(DBLOCK)                                    
MTHFCTR  DS    F                   WEIGHTING FACTOR FOR X AND /                 
MTHIFIL  DS    CL3                 INPUT FILE                                   
MTHOFIL  DS    CL3                 OUTPUT FILE                                  
MTHOSRC  DS    CL3                 OUTPUT SOURCE                                
         SPACE 2                                                                
ADDSW    DS    C                   Y=ADD DEMOS ONLY                             
IUNSW    DS    C                   Y=INVENTORY REC IN IUN FORMAT                
INDEXUP  DS    C                   Y=INV. REC HAS BEEN INDEX UPGRADED           
U191     DS    X                   X'80' - THERE IS NO U191                     
*                                  X'40' - THERE IS U191                        
DEMCOD   DS    CL3                                                              
         DS    0F                                                               
       ++INCLUDE DEDBLOCK                                                       
PROGEL   DS    CL60                PROGRAM NAME ELEMENT                         
         SPACE 2                                                                
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
*                                  X'04'  USER WEIGHTING OVERRIDE               
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL4                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         SPACE 2                                                                
* PROGRAM NAME TEXT ELEMENT DSECT                                               
*                                                                               
PROGELD  DSECT                                                                  
PCODE    DS    X                                                                
PELLEN   DS    X                                                                
PLIN     DS    X                                                                
         DS    CL3                 SPARE                                        
PFBK     DS    0CL5                FROM BOOK                                    
PMON     DS    CL3                                                              
PYR      DS    CL2                                                              
         DS    CL1                 SPARE-BLANK                                  
PINVCODE DS    CL2                                                              
PEQS     DS    CL1                                                              
PNAME1   DS    CL16                FIRST PROGRAM NAME                           
PNAME2   DS    CL16                SECOND PROGRAM NAME                          
PROGELX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
*        DEMO RECORD IUN DSECT FOR USE BY FIXPAV                                
*                                                                               
IUNREC   DSECT                                                                  
UPREC    DS    0F                                                               
***********************************************************************         
*                                  ORIGINAL BOOK VALUES               *         
***********************************************************************         
OLDUNV   DS    (NUMVALS)F          UNIVERSES                          *         
OLDUNVX  EQU   *                                                      *         
***********************************************************************         
OLDRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                      *         
         ORG                                                                    
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
OLDRTGX  EQU   *                                                      *         
***********************************************************************         
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                      *         
         ORG                                                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                      *         
         ORG                                                                    
OLDHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  NEW VALUES                         *         
NEWUNV   EQU   OLDTOT              DEFINE ORIGIN FOR REGETIUN CALL    *         
*                                                                     *         
***********************************************************************         
NEWRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                      *         
         ORG                                                                    
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
NEWRTGX  EQU   *                                                      *         
***********************************************************************         
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                      *         
         ORG                                                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
NEWHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  OTHER VALUES                       *         
***********************************************************************         
HOMESHR  DS    3F                  ORIGINAL HOMES SHARES              *         
HOMSHRX  EQU   *                                                      *         
HOMSHRLN EQU   *-HOMSHR                                               *         
***********************************************************************         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                       *         
LUNVX    EQU   *                                                      *         
***********************************************************************         
UPRECX   DS    0F                                                               
*                                                                               
NUMVALS  EQU   32                                                               
DISPHOM  EQU   20                                                               
LENVALS  EQU   NUMVALS*4                                                        
         EJECT                                                                  
       ++INCLUDE REBKLSTD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'223RERMP11A  05/01/02'                                      
         END                                                                    
