*          DATA SET ACREPCZ02  AT LEVEL 011 AS OF 12/16/15                      
*PHASE ACCZ02A                                                                  
*INCLUDE CENTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'CHECK AUTHORIZATION FILE RECORD REPORT PROGRAM'                 
***********************************************************************         
*        UPDATE HISTORY:                                              *         
*                                                                     *         
*        03/25/97 - LSS - 1. REPORT ON ALL ACCPAK SYSTEMS IN ONE RUN  *         
*                         2. OPTION TO REPORT ONLY ON IDS WHERE THE   *         
*                            LOCAL OR SOON DATE IS MORE THAN N DAYS   *         
*                            AGO - N IS DEFINED IN QSELECT.           *         
*                                                                     *         
***********************************************************************         
ACCZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCZ**,R9,R8                                                 
         L     RC,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RC                                                       
*                                                                               
         LA    RA,SPACEND                                                       
*                                                                               
         USING ACCZD,RA                                                         
*                                                                               
         ST    RA,CZBASEA                                                       
         ST    RB,CZBASEB                                                       
         ST    RC,CZBASEC                                                       
*                                                                               
         CLI   MODE,RUNFRST        RUN   FIRST                                  
         BE    RUNF                                                             
         CLI   STOPSW,YES          STOP  RUN ?                                  
         BE    XIT                 YES,  EXIT                                   
         CLI   MODE,REQFRST        REQUEST    FIRST                             
         BE    REQF                                                             
         CLI   MODE,RUNLAST        RUN   LAST                                   
         BE    RUNL                                                             
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        F I R S T   F O R   R U N                                    *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   DATADISP,=Y(ACCRFST-ACCRECD)                                     
         MVI   COMPANY,X'00'       CLEAR COMPANY CODE                           
*                                                                               
         USING MASTD,RE                                                         
         L     RE,ADMASTC                                                       
         MVC   UPSI,MCUPSI         GET UPSI SWITCHES                            
         DROP  RE                                                               
*                                                                               
         MVI   DEBUGSW,X'00'       TURN OFF DEBUGGING                           
         TM    UPSI,X'80'          DEBUGGING REQUESTED?                         
         BZ    *+8                 NO, SKIP                                     
         MVI   DEBUGSW,YES         TURN ON DEBUGGING                            
*                                                                               
         L     RE,=A(BXHOOK)       BOX HOOK                                     
         ST    RE,HEADHOOK                                                      
*                                                                               
         MVI   SPACES,SPACE        INITIALIZE SPACES                            
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVI   PRTOPTS1,PRTBOXES   PRINT BOXES                                  
         MVI   PRTOPTS2,X'00'                                                   
*                                  PRINT NEW PAGE+PRINT FIRST                   
         MVI   PRTSWS,PRTNEWPG+PRTFIRST                                         
         MVC   PAGEWDTH,=A(PGWDRL15)                                            
         MVI   MAXLINES,SHORTPG    MAX LINES PER PAGE                           
         BAS   RE,INITH            INTIALIZE THE HEADER AREA                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        F I R S T  F O R  R E Q U E S T                              *         
***********************************************************************         
         SPACE 1                                                                
REQF     ZAP   PAGENUM,ZEROS       PAGE NUMBER                                  
         MVI   LINE,INITLN         PRINT LINE                                   
         MVC   PAGE,=H'01'         PAGE 1                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING MASTD,RE                                                         
         L     RE,ADMASTC                                                       
         L     R2,MCUTL            GET AND AVE UTL                              
         ST    R2,AUTL                                                          
         MVC   SAVESE,4(R2)        SAVE SE ID FOR CTL FILES                     
         DROP  RE                                                               
*                                                                               
         XC    DISPB4#,DISPB4#     INITIALIZE DISPLAY B4 NUMBER                 
         CLC   QSELECT,SPACES      ANY QSELECT INPUT?                           
         BE    REQF00              NO, SKIP                                     
         GOTO1 AGETNUM,PARMLIST,(L'QSELECT,QSELECT)                             
         CLI   PARMLIST,X'00'      VALID NUMBER?                                
         BNE   ERR1                NO, INVALID QSELECT                          
         ICM   R2,15,PARMLIST+4    GET THE VALUE                                
         BM    ERR1                MINUS,INVALID QSELECT                        
         BZ    REQF00              ZERO, SKIP                                   
         MH    R2,=H'-1'           TIMES MINUS ONE                              
         ST    R2,DISPB4#          SAVE QSELECT                                 
*                                  GET TODAY YYMMDD                             
         GOTO1 DATCON,PARMLIST,(5,0),(0,TODAYBCD)                               
         GOTO1 ADDAY,PARMLIST,TODAYBCD,B4DAYBCD,(R2)                            
         GOTO1 DATCON,PARMLIST,(0,B4DAYBCD),(2,B4DAYCMP)                        
*                                                                               
REQF00   XC    SKEY,SKEY           CLEAR SYS LIST REC  KEY                      
         USING CTWREC,R6                                                        
         LA    R6,SKEY             SYSTEM LIST KEY                              
         MVI   CTWKTYP,CTWKTYPQ    REC TYPE C'W' SYSTEM LISTS                   
         MVI   CTWKREC,CTWKRSYS    SUBREC TYPE C'S' SYSTEM LIST                 
         MVI   MSTRT,0             INITIALIZE SMTP                              
*MNFIL                                                                          
         USING MASTD,RE                                                         
         L     RE,ADMASTC                                                       
         CLI   MCTSTRUN,X'FF'                                                   
         BNE   *+8                                                              
         MVI   MSTRT,X'FF'         SUPPRESS ANY SMTP EMAILS                     
         DROP  RE                                                               
*MNFIL                                                                          
*                                                                               
         B     REQF05                                                           
*                                                                               
REQFNXAC TM    READSEQ,READSSEQ    READ SEQUENTIAL?                             
         BO    REQF10              YES, SKIP READ HIGH                          
*                                                                               
REQF05   LA    R0,6                DEBUG SKEY                                   
         BAS   RE,DUMP             DUMP IT                                      
         BAS   RE,DMCTHGH          READ HIGH                                    
         L     R6,AIOSL            I/O AREA                                     
         CLC   SKEY(CTWDATA-CTWREC),0(R6)                                       
         BNE   REQF15              NO, GOT NEXT SYS LIST RECORD                 
*                                                                               
REQF10   LA    R0,6                DEBUG SKEY                                   
         BAS   RE,DUMP             DUMP IT                                      
         BAS   RE,DMCTSEQ                                                       
*                                                                               
REQF15   OI    READSEQ,READSSEQ    SO FAR CAN READ SEQUENTIAL                   
         L     R6,AIOSL            I/O AREA                                     
         MVC   SKEY(CTWDATA-CTWREC),0(R6)                                       
         LA    R0,7                DEBUG AIOSL                                  
*                                                                               
         BAS   RE,DUMP             DUMP IT                                      
         CLI   CTWKTYP,CTWKTYPQ    SYSTEM LIST RECORD?                          
         BNE   REQFEX              NO, EXIT                                     
         CLI   CTWKREC,CTWKRSYS    SYSTEM LIST?                                 
         BNE   REQF30              NO, GET NEXT RECORD                          
         CLI   CTWKSYSN,CTWKACC    SYSTEM NUMBER=ACCPAK?                        
         BNE   REQF30              NO, GET NEXT RECORD                          
*                                                                               
         USING CTLSTD,R5           LIST ELEMENT DSECT                           
         LA    R5,CTWDATA          ELEMENTS                                     
*                                                                               
REQF20   CLI   0(R5),X'00'         END OF RECORD?                               
         BE    REQF30              YES, GET NEXT RECORD                         
         CLI   CTLSTEL,CTLSTELQ    X'A4' ELEMENT?                               
         BNE   REQF25              NO, GET NEXT ELEMENT                         
         CLI   CTLSTDTA+7,CTWKACC  ACCPAK LIST DATA?                            
         BNE   REQF25              NO, GET NEXT ELEMENT                         
         MVC   SYSSE,CTLSTDTA+8    SAVE SE NUMBER                               
         MVC   ACCSYSID,CTLSTDTA   SAVE ACCPAK SYSTEM ID                        
         L     R2,AUTL             UTL                                          
         MVC   4(1,R2),SYSSE       SET UTL FOR THIS ACCPAK FILE                 
         L     R2,AIOSL                                                         
         BAS   RE,DMOPNACC         OPEN ACC FILE                                
         NI    READSEQ,X'FF'-READSSEQ                                           
         BAS   RE,GCHKREC          GET FIRST CHECK RECORD                       
         L     R2,AUTL             UTL                                          
         MVC   4(1,R2),SAVESE      RESTORE SE ID FOR CTL FILES                  
*                                                                               
REQF25   ZIC   R2,1(,R5)           CURR ELEMENT LENGTH                          
         AR    R5,R2               NEXT ELEMENT ADDR                            
         B     REQF20              TRY NEXT ELEMENT                             
*                                                                               
REQF30   B     REQFNXAC            GET NEXT SEQUENTIAL RECORD                   
*                                                                               
REQFEX   CLI   MSTRT,0             ANY EMAILS?                                  
         BE    XIT                 NO                                           
*MNFIL                                                                          
         CLI   MSTRT,X'FF'         SUPPRESS EMAILS RUN=TEST                     
         BE    XIT                 NO                                           
*MNFIL                                                                          
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT ,                                                                
         USING CHARECD,R6                                                       
GCHKREC  NTR1  ,                   FIND 1ST CHECK RECORD                        
         LA    R6,DKEY                                                          
         MVC   CHAKEY,SPACES                                                    
         MVI   CHAKTYP,CHAKTYPQ    CHECK AUTHORIZATION RECORD                   
         XC    CHAKCULA,CHAKCULA   CLEAR COMPANY CODE, UNIT/LEDGER              
         B     GCHK10              READ KEY HIGH                                
*                                                                               
GCHKNEXT TM    READSEQ,READ0SEQ    READ DIR SEQUENTIAL?                         
         BO    GCHK20              YES, SKIP READ HIGH                          
*                                                                               
GCHK10   SR    R0,R0               DEBUG DKEY                                   
         BAS   RE,DUMP             DUMP IT                                      
*                                                                               
         BAS   RE,DMHGH            READ HIGH                                    
         CLC   DKEY,DIR            SAME KEY?                                    
         BNE   GCHK30              NO, GOT NEXT DIRECTORY RECORD                
*                                                                               
GCHK20   SR    R0,R0               DEBUG DKEY                                   
         BAS   RE,DUMP             DUMP IT                                      
         BAS   RE,DMSEQ                                                         
*                                                                               
GCHK30   OI    READSEQ,READ0SEQ    SO FAR CAN READ SEQUENTIAL                   
         MVC   DKEY,DIR            SAVE DIRECTORY KEY                           
         LA    R0,1                DEBUG DIR                                    
         BAS   RE,DUMP             DUMP IT                                      
         BAS   RE,DMGETR           GET RECORD                                   
         LA    R0,2                DEBUG AIO                                    
         BAS   RE,DUMP             DUMP IT                                      
*                                                                               
         L     R6,AIO              POINT RECORD AREA                            
         MVC   DKEY,CHAKEY         SAVE DIRECTORY KEY                           
         CLI   CHAKTYP,CHAKTYPQ    CHECK AUTHORIZATION RECORD?                  
         BNE   GCHKEX              NO, EXIT                                     
*                                                                               
*                                  ************************************         
*                                  * FUTURE EXTENSION:                *         
*                                  *     ADD CODE HERE TO TEST FOR    *         
*                                  *     FILTERED OUT COMPANY -       *         
*                                  *     IF FILTERED OUT,             *         
*                                  *        BRANCH TO GCHKNEXT        *         
*                                  ************************************         
*                                                                               
*                                  ************************************         
*                                  * FUTURE EXTENSION:                *         
*                                  *     ADD CODE HERE TO TEST FOR    *         
*                                  *     FILTERED OUT LEDGER -        *         
*                                  *     IF FILTERED OUT,             *         
*                                  *        BRANCH TO GCHKNEXT        *         
*                                  ************************************         
*                                                                               
         MVC   SAVEUNLD(2),CHAKUNT SAVE THE UNIT/LEDGER                         
         CLC   COMPANY,CHAKCPY     IS THIS ANOTHER COMPANY CODE?                
         BE    GCHK80              NO, SKIP TO FORMAT                           
         CLI   MSTRT,2                                                          
         BNE   *+12                                                             
         MVI   MSTRT,3                                                          
         BAS   RE,SENDMAIL         SEND WHAT WE HAVE                            
*                                                                               
GCHK35   OI    PRTSWS,PRTNEWPG     PRINT NEW PAGE                               
         MVC   COMPANY,CHAKCPY     SAVE COMPANY CODE                            
*                                                                               
         USING HEADD,R2                                                         
         L     R2,AHEAD            HEADERS                                      
         ZIC   R3,CHAKCPY                                                       
         SRL   R3,4                DROP LOW ORDER BYTE                          
         IC    R3,HEXTABLE(R3)                                                  
         STC   R3,HDLCPYCD         SAVE HIGH ORDER BYTE                         
         IC    R3,CHAKCPY                                                       
         N     R3,=F'15'           DROP HIGH ORDER BYTE                         
         IC    R3,HEXTABLE(R3)                                                  
         STC   R3,HDLCPYCD+1       SAVE LOW ORDER BYTE                          
*                                                                               
*                                  TURN OFF READ DIR SEQ                        
         NI    READSEQ,X'FF'-READ0SEQ                                           
         MVC   DKEYSV,DKEY         SAVE KEY                                     
         XC    DKEY,DKEY                                                        
         MVC   DKEY(1),COMPANY     READ COMPANY DIR RECORD                      
         BAS   RE,DMRD                                                          
*                                                                               
         LA    R0,1                DEBUG DIR                                    
         BAS   RE,DUMP             DUMP IT                                      
*                                                                               
         BAS   RE,DMGETR2          READ COMPANY RECORD                          
*                                                                               
         LA    R0,3                DEBUG DIR                                    
         BAS   RE,DUMP             DUMP IT                                      
*                                                                               
         L     R3,AIO2             COMPANY RECORD                               
         MVC   HDLCPY,SPACES       CLEAR COMPANY NAME                           
         AH    R3,DATADISP                                                      
         SR    R4,R4                                                            
*                                                                               
         USING NAMELD,R3                                                        
GCHK40   CLI   NAMEL,0             ANY MORE ELEMENTS?                           
         BE    GCHK70              NO, SKIP COMPANY NAME                        
         CLI   NAMEL,NAMELQ        NAME ELEMENT?                                
         BE    GCHK50              YES, GET NAME                                
         IC    R4,NAMLN            GET ELEMENT LENGTH                           
         AR    R3,R4               NEXT ELEMENT                                 
         B     GCHK40                                                           
*                                                                               
GCHK50   IC    R4,NAMLN            GET ELEMENT LENGTH                           
         SH    R4,=AL2(NAMLN1Q+1)  SET UP FOR EXMVC                             
         EX    R4,*+8              MOVE TO HEADER                               
         B     GCHK60                                                           
         MVC   HDLCPY(0),NAMEREC   MOVE ELEMENT NAME                            
*                                                                               
GCHK60   GOTO1 CENTER,PARMLIST,HDLCPY,L'HDLCPY                                  
         DROP  R2,R3                                                            
*                                                                               
GCHK70   MVC   DKEY,DKEYSV         RESTORE KEY                                  
*                                                                               
GCHK80   BAS   RE,FORMAT           FORMAT THE DETAIL AREA                       
         B     GCHKNEXT            READ NEXT CHECK RECORD                       
*                                                                               
GCHKEX   CLI   MSTRT,2             ANY EMAIL FORMATTED?                         
         BNE   XIT                 NO                                           
         MVI   MSTRT,3             YES                                          
         BAS   RE,SENDMAIL         SEND IT NOW                                  
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*        FORMAT THE DETAIL AREA                                       *         
***********************************************************************         
*                                                                               
         USING DETAILD,R2          DETAIL LINE                                  
         USING OCNELD,R4           OFFICE CHECK NUMBER ELEMEMT                  
FORMAT   NTR1  ,                   FORMAT THE DETAIL AREA                       
         L     R2,ADETAIL          ->DETAIL LINE                                
         L     R4,AIO              ->CHECK AUTHORIZATION RECORD                 
         AH    R4,DATADISP                                                      
*                                                                               
FORM10   DS    0H                                                               
         CLI   OCNEL,0             END OF RECORD?                               
         BE    FORMRECX            YES, NEXT RECORD                             
         LA    R0,4                DEBUG OCN                                    
         BAS   RE,DUMP             DUMP IT                                      
         CLI   OCNEL,OCNELQ        CHECK NUMBER ELEMENT?                        
         BNE   FORM80              NO, PROCESS NEXT ELEMENT                     
*                                                                               
         MVC   0(PGWDRL15,R2),SPACESX                                           
         MVI   DISPB4SW,NO         BEFORE NOT FOUND SWITCH                      
         MVC   EDETAIL,SPACES                                                   
         MVC   EDETAIL2,SPACES                                                  
*                                                                               
*                                  ************************************         
*                                  * FUTURE EXTENSION:                *         
*                                  *     ADD CODE HERE TO TEST FOR    *         
*                                  *     FILTERED OUT ELEMENT:        *         
*                                  *                                  *         
*                                  *     . OFFICE  FILTER             *         
*                                  *     . CLIENT  FILTER             *         
*                                  *     . POSTING OFFICE             *         
*                                  *     . CURRENT STATUS             *         
*                                  *                                  *         
*                                  *     BRANCH FORM80 IF FILTERED OUT*         
*                                  ************************************         
*                                                                               
         L     R1,AHEAD                                                         
         USING HEADD,R1                                                         
         MVC   EDCPY,HDLCPYCD                                                   
         DROP  R1                                                               
         MVC   EDACC,ACCSYSID                                                   
*                                                                               
         MVC   DETUNLD,SAVEUNLD    INSERT LEDGER                                
         MVC   EDUL,DETUNLD                                                     
*                                                                               
         BAS   RE,GETID            GET THE ID                                   
         MVC   DETID,WORKID        INSERT THE ID                                
         MVC   EDID,DETID                                                       
*                                                                               
         MVC   DETSCHK,OCNBEF      INSERT THE STARTING CHECK NUM                
         CLI   OCNNTYP,0           OPTIONAL NUMBER TYPE?                        
         BE    *+10                                                             
         MVC   DETNCHK(1),OCNNTYP  NUMBER TYPE                                  
         MVC   DETNCHK,OCNAFT      INSERT THE LAST CHECK NUM                    
         CLI   OCNNTYP,0           OPTIONAL NUMBER TYPE?                        
         BE    *+10                                                             
         MVC   DETNCHK(1),OCNNTYP  NUMBER TYPE                                  
         MVC   EDSTRT,DETSCHK                                                   
         MVC   EDNEXT,DETNCHK                                                   
*                                                                               
         MVC   DETBANK,OCNBANKA    BANK ACCOUNT                                 
         CLC   OCNBANKU(2),=C'SC'                                               
         BE    *+10                                                             
         MVC   DETBANK,OCNBANK     OFFICE/LEDGER BANK ACCOUNT                   
         MVC   EDBANK,DETBANK                                                   
*                                                                               
         MVC   DETFILT,OCNFILT     CLIENT/OFFICE FILTER                         
         OC    DETFILT,SPACES                                                   
         MVC   EDFILT,DETFILT                                                   
*                                                                               
         MVC   DETLCHK,OCNLAST     LAST CHECK NUMBER                            
         OC    DETLCHK,SPACES      MAKE READABLE                                
         MVC   EDLCHK,DETLCHK                                                   
*                                                                               
         XR    R5,R5               USED IN STUN ROUTINE                         
         STH   R5,STAOFF           STATUS FIELD OFFSET                          
*                                                                               
         LA    R6,OCNSTAT          R6=STATUS BYTE                               
         L     R1,ASTALST          R1=STATUS TABLE                              
         BAS   RE,STUN             UNSCAN THE STATUS BYTE                       
*                                                                               
         CLI   OCNLN,OCNLN3Q       MORE STATUS BYTES AVAILABLE?                 
         BL    FORM60              NO, SKIP                                     
*                                                                               
         LA    R6,OCNSTAT2         R6=STATUS BYTE                               
         L     R1,ASTALST2         R1=STATUS TABLE                              
         BAS   RE,STUN             UNSCAN THE STATUS BYTE                       
*                                                                               
         LA    R6,OCNLASR          R6=STATUS BYTE                               
         L     R1,ASTALASR         R1=STATUS TABLE                              
         BAS   RE,STUNLASR         UNSCAN THE STATUS BYTE                       
*                                                                               
         L     RF,ASORTS           ADDR OF SORTS TABLE                          
*                                                                               
FORM30   CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    FORM40              YES, CONTINUE                                
         CLC   OCNSORT,0(RF)       FOUND SORTS OPTION?                          
         BE    FORM35              YES, USE MATCH                               
         LA    RF,L'SORTS(,RF)     NO, NEXT ENTRY                               
         B     FORM30              LOOP                                         
*                                                                               
FORM35   MVC   DETSORT(L'SORTS-1),1(RF)                                         
         MVC   EDSORT,DETSORT                                                   
*                                                                               
FORM40   MVC   DETPOFF,OCNPOFF     POSTING OFFICE                               
         OC    DETPOFF,SPACES      UPPER CASE                                   
         MVC   EDPOFF,DETPOFF                                                   
*                                                                               
         OC    OCNSEQN,OCNSEQN     STACK SEQUENCE NUMBER?                       
         BZ    FORM60              NO, SKIP                                     
*                                                                               
         LH    R0,STAOFF           STATUS OFFSET                                
         LA    R3,DETSTAT          STATUS FIELD                                 
         AR    R3,R0               NEXT AVAILABLE BYTE                          
         LTR   R0,R0               ANY DATA SO FAR?                             
         BZ    FORM45              NO, SKIP                                     
         MVI   0(R3),C','          INSERT COMMA                                 
         AH    R0,=H'01'           ADD ONE TO OFFSET                            
*                                  ADD ONE TO NEXT AVAILABLE BYTE               
         LA    R3,1(,R3)                                                        
*                                                                               
FORM45   MVC   0(5,R3),=CL5'STACK' INSERT DATA                                  
         AH    R0,=H'05'           ADD DATA LENGTH                              
         LA    R3,5(,R3)           NEXT AVAILABLE BYTE                          
         MVI   0(R3),C'='          INSERT EQUAL SIGN                            
         AH    R0,=H'01'           ADD ONE TO OFFSET                            
*                                  ADD ONE TO NEXT AVAILABLE BYTE               
         LA    R3,1(,R3)                                                        
         MVC   EDSTAT,DETSTAT                                                   
*                                                                               
         SR    R1,R1               CLEAR REGISTER                               
         ICM   R1,3,OCNSEQN        SEQUENCE NUMBER                              
         CVD   R1,STAWORK          CONVERT TO DECIMAL                           
         OI    STAWORK+7,X'0F'                                                  
         UNPK  0(5,R3),STAWORK+5(3)                                             
         AH    R0,=H'05'           ADD DATA LENGTH                              
         LA    R3,5(,R3)           NEXT AVAILABLE BYTE                          
         STH   R0,STAOFF           SAVE OFFSET                                  
*                                                                               
FORM60   OC    DISPB4#,DISPB4#     QSELECT NUMBER PROVIDED?                     
         BZ    FORM65              NO, SKIP NEXT TEST                           
         CLI   DISPB4SW,YES        FOUND BEFORE DATE?                           
         BNE   FORM80              NO, SKIP THIS ELEMENT                        
*                                                                               
FORM65   BAS   RE,PRINTEL          PRINT THE DATA ELEMENT                       
         BAS   RE,SENDMAIL         SEND AN EMAIL                                
*                                                                               
FORM80   ZIC   R3,OCNLN            GET ELEMENT LENGTH                           
         AR    R4,R3               ->NEXT ELEMENT                               
         B     FORM10                                                           
*                                                                               
FORMRECX B     XIT                 READ  NEXT CHECK     RECORD                  
         EJECT ,                                                                
***********************************************************************         
*        L A S T  F O R  R U N                                        *         
***********************************************************************         
*        SPACE 1                                                                
RUNL     DS    0H                                                               
         MVI   FORCEHED,NO                                                      
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
*                                                                               
DMRD     DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMGETR2  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO2,DMWORK                        
         B     DMERR                                                            
*                                                                               
DMIDHGH  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,AIO3,AIO3                             
         B     DMERR                                                            
*                                                                               
DMCTHGH  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,SKEY,AIOSL                            
         B     DMERR                                                            
*                                                                               
DMCTSEQ  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,CTFILE,SKEY,AIOSL                            
         B     DMERR                                                            
*                                                                               
DMOPNACC DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMOPEN,ACCOUNT,ACFILEL                              
         B     DMERROK                                                          
*                                                                               
DMERR    DS    0H                                                               
         MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    DMERROK                                                          
         DC    H'0'                                                             
*                                                                               
DMERROK  DS    0H                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*        GETID - READ CONTROL FILE FOR ID                             *         
*                                                                     *         
*              ON INPUT:                                              *         
*                 R4 = ADDRESS OF OCNELD                              *         
***********************************************************************         
*                                                                               
         USING DETAILD,R2                                                       
         USING OCNELD,R4           OFFICE     CHECK     NUMBER  ELEMEMT         
         USING CTIREC,R5                                                        
         SPACE 1                                                                
GETID    DS    0H                                                               
         NTR1  ,                                                                
         L     RE,AUTL             ->    UTL                                    
         MVC   4(1,RE),SAVESE      RESTORE    SE   ID   FOR  UTL  FILES         
*                                                                               
         L     R2,ADETAIL          ->    DETAIL    AREA                         
         L     R5,AIO3             ->    THIRD     BUFFER    AREA               
         NI    READSEQ,X'FF'-READ0SEQ                                           
         XC    CTIKEY,CTIKEY       CLEAR KEY  AREA                              
         MVI   CTIKEY,CTIKTYPQ     ID    RECORD   (C'I')                        
         MVC   CTIKNUM,OCNOFFID    ID    NUMBER                                 
         MVC   CTSAVE,CTIKEY                                                    
*                                                                               
         LA    R0,5                DEBUG CTFILE                                 
         BAS   RE,DUMP             DUMP  IT                                     
*                                                                               
         BAS   RE,DMIDHGH          READ  ID   RECORD                            
*                                                                               
         LA    R0,5                DEBUG CTFILE                                 
         BAS   RE,DUMP             DUMP  IT                                     
*                                                                               
         L     RE,AUTL             ->    UTL                                    
         MVC   4(1,RE),SYSSE       RESTORE    THIS ACCPAK    SE   ID            
*                                                                               
         MVC   WORKID,SPACES                                                    
         CLC   CTSAVE,CTIKEY       SAME  KEY  ?                                 
         BNE   XIT                                                              
         LA    R5,CTIDATA          FIND  ID   ELEMENT                           
         SR    R0,R0                                                            
*                                                                               
GET8     DS    0H                                                               
         CLI   0(R5),0             NO    ELEMENT ?                              
         BE    XIT                                                              
         CLI   0(R5),CTDSCELQ      X'02' ELEMENT ?                              
         BE    GET10                                                            
         IC    R0,1(,R5)                                                        
         AR    R5,R0                                                            
         B     GET8                                                             
*                                                                               
GET10    DS    0H                  FOUND ID   ELEMENT                           
         MVC   WORKID,2(R5)                                                     
         B     XIT                                                              
*                                                                               
         DROP  R2,R4,R5                                                         
         EJECT ,                                                                
***********************************************************************         
*        INITIALIZE THE HEADERS AREA                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING HEADD,R2                                                         
         SPACE 1                                                                
INITH    DS    0H                                                               
         NTR1                                                                   
         L     R2,AHEAD            ->    HEADER    AREA                         
         LA    R3,HDLLINES         NUMBER     OF   HEADER    LINES              
         LR    R4,R2               ->    HEADER   AREA                          
*                                                                               
INITH10  DS    0H                                                               
*                                  CLEAR A    HEADER    LINE                    
         MVC   0(PGWDRL15,R4),SPACESX                                           
         LA    R4,PGWDRL15(,R4)    ->    NEXT LINE                              
         BCT   R3,INITH10          CLEAR NEXT LINE                              
*                                                                               
         MVC   HDLCPYCX(8),=C'COMPANY:'                                         
         MVC   HDLPAGEX(5),=C'PAGE:'                                            
         MVC   HDLACSYS(8),=C'ACC SYS:'                                         
         MVC   HDLIDX(2),=C'ID'                                                 
         MVC   HDLIDXX(2),=C'--'                                                
         MVC   HDLUNITX(4),=C'UNIT'                                             
         MVC   HDLLDGRX(6),=C'LEDGER'                                           
         MVC   HDLSTRTX(5),=C'START'                                            
         MVC   HDLCHCK1(5),=C'CHECK'                                            
         MVC   HDLNEXTX(4),=C'NEXT'                                             
         MVC   HDLCHCK2(5),=C'CHECK'                                            
         MVC   HDLBANKX(4),=C'BANK'                                             
         MVC   HDLACNTX(7),=C'ACCOUNT'                                          
         MVC   HDLOFCLX(13),=C'OFFICE/CLIENT'                                   
         MVC   HDLFILTX(6),=C'FILTER'                                           
         MVC   HDLLASTX(4),=C'LAST'                                             
         MVC   HDLCHCK3(5),=C'CHECK'                                            
         MVC   HDLSORTX(4),=C'SORT'                                             
         MVC   HDLSEQX(8),=C'SEQUENCE'                                          
         MVC   HDLPOSTX(7),=C'POSTING'                                          
         MVC   HDLOFFX(6),=C'OFFICE'                                            
         MVC   HDLSTATX(14),=C'CURRENT STATUS'                                  
         B     XIT                 RETURN                                       
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        STUN  - UNSCAN THE STATUS LINE                               *         
*                                                                     *         
*        INPUT:                                                       *         
*               R2       - DETAILD                                    *         
*               R4       - OCNELD                                     *         
*               R6       - STATUS SWITCH                              *         
*               SAVTADDR - STATUS TABLE                               *         
*               STAOFF   - STATUS OFFSET                              *         
*                                                                     *         
*        OUTPUT:                                                      *         
*               STAOFF   - STATUS OFFSET                              *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING DETAILD,R2          DETAIL     LINE                              
         USING OCNELD,R4           OFFICE     CHECK     NUMBER ELEMENT          
         SPACE 1                                                                
STUN     DS    0H                                                               
         NTR1  ,                                                                
*                                                                               
STUN10   DS    0H                                                               
         MVC   STABYTE,1(R1)       GET   BIT  FROM TABLE                        
         NC    STABYTE,0(R6)       IS    BIT  ON   IN   STATUS FIELD ?          
         BZ    STUN80              NO,   SKIP                                   
         CLC   2(10,R1),=CL10'MICR'                                             
         BNE   STUN20                                                           
         CLI   OCNLASR,0           LASAR PRINTING  ?                            
         BNE   STUN80              YES,  SKIP                                   
*                                                                               
STUN20   DS    0H                                                               
         BAS   RE,STINSERT         INSERT     THE  STATUS   FIELD               
*                                                                               
STUN80   DS    0H                                                               
         LA    R1,L'STATLST(,R1)   NEXT  TABLE     ENTRY                        
         CLI   0(R1),X'FF'         TABLE DONE ?                                 
         BNE   STUN10              NO,   PROCESS   NEXT ENTRY                   
         B     XIT                 RETURN                                       
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        STUNLASR - UNSCAN THE STATUS LINE FOR LASER PRINTING COLORS  *         
*                                                                     *         
*        INPUT:                                                       *         
*               R2       - DETAILD                                    *         
*               R4       - OCNELD                                     *         
*               R6       - STATUS SWITCH                              *         
*               SAVTADDR - STATUS TABLE                               *         
*               STAOFF   - STATUS OFFSET                              *         
*                                                                     *         
*        OUTPUT:                                                      *         
*               STAOFF   - STATUS OFFSET                              *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING DETAILD,R2          DETAIL     LINE                              
         USING OCNELD,R4           OFFICE     CHECK     NUMBER ELEMENT          
         SPACE 1                                                                
STUNLASR DS    0H                                                               
         NTR1  ,                                                                
*                                                                               
STUL10   DS    0H                                                               
         MVC   STABYTE,1(R1)       GET   BYTE FROM TABLE                        
         CLC   STABYTE,0(R6)       IS    BYTE SAME IN   STATUS FIELD ?          
         BNE   STUL80              NO,   SKIP                                   
*                                                                               
         BAS   RE,STINSERT         INSERT     THE  STATUS   FIELD               
         B     STUL90              EXIT, NO   MORE MATCHES  POSSIBLE            
*                                                                               
STUL80   DS    0H                                                               
         LA    R1,L'STATLASR(,R1)  NEXT  TABLE     ENTRY                        
         CLI   0(R1),X'FF'         TABLE DONE ?                                 
         BNE   STUL10              NO,   PROCESS   NEXT ENTRY                   
*                                                                               
STUL90   DS    0H                                                               
         B     XIT                 RETURN                                       
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        STINSERT - INSERT THE STATUS FOR STUN AND STUNM ROUTINES     *         
*                                                                     *         
*        INPUT:                                                       *         
*               R1       - MATCH  STATUS TABLE  ENTRY                 *         
*               R2       - DETAILD                                    *         
*               R4       - OCNELD                                     *         
*               DETSTAT  - DETAIL LINE   STATUS FIELD                 *         
*               STAOFF   - STATUS OFFSET                              *         
*                                                                     *         
*        WORK AREA:                                                   *         
*               SAVTADDR - STATUS TABLE  ENTRY                        *         
*               STAWORK  - CURRENT       ENTRY  CONSTANT              *         
*                                                                     *         
*        OUTPUT:                                                      *         
*               DETSTAT  - DETAIL LINE   STATUS FIELD                 *         
*               STAOFF   - STATUS OFFSET                              *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING DETAILD,R2          DETAIL     LINE                              
         USING OCNELD,R4           OFFICE     CHECK     NUMBER ELEMENT          
         SPACE 1                                                                
STINSERT DS    0H                                                               
         NTR1  ,                                                                
         ST    R1,SAVTADDR         SAVE  TABLE     ENTRY     ADDRESS            
         MVC   STAWORK,SPACES      CLEAR WORK AREA                              
         MVC   STAWORK(10),2(R1)   INSERT     CONSTANT                          
         LA    R3,STAWORK+9        LAST  BYTE OF   DATA                         
         LA    R7,9                LENGTH     OF   DATA - 1                     
*                                                                               
STIN05   DS    0H                                                               
         CLI   0(R3),C' '          BLANK ?                                      
         BNE   STIN10              NO,   CONTINUE                               
         BCTR  R3,0                PREVIOUS   BYTE                              
         BCT   R7,STIN05           CHECK IT                                     
*                                                                               
STIN10   DS    0H                                                               
         LH    R0,STAOFF           STATUS     OFFSET                            
         LA    R3,DETSTAT          STATUS     FIELD                             
         AR    R3,R0               NEXT  AVAILABLE BYTE                         
         LTR   R0,R0               ANY   DATA SO   FAR  ?                       
         BZ    STIN15              NO,   SKIP                                   
         MVI   0(R3),C','          INSERT     COMMA                             
         AH    R0,=H'01'           ADD   ONE  TO   OFFSET                       
         LA    R3,1(,R3)           ADD   ONE  TO   NEXT AVAILABLE BYTE          
*                                                                               
STIN15   DS    0H                                                               
         EXMVC R7,0(R3),STAWORK    INSERT     DATA                              
         AR    R0,R7               PLUS  EXMVC     LENGTH                       
         AH    R0,=H'01'           PLUS  ONE                                    
         STH   R0,STAOFF           SAVE  OFFSET                                 
         LA    R3,1(R7,R3)         NEXT  AVAILABLE BYTE                         
*                                                                               
         CLI   0(R1),X'00'         ANY   SECONDARY DATA ?                       
         BE    STIN80              NO,   SKIP                                   
*                                                                               
*        CODE FOR SECONDARY DATA                                                
*                                                                               
         CLC   STAWORK(10),=CL10'SOON'                                          
         BNE   STIN20                                                           
         OC    OCNDPSR,OCNDPSR     SOON  REGISTER  PENDING ?                    
         BZ    STIN40              NO,   SKIP                                   
         LA    RF,OCNDPSR          DATE  OF   PENDING SOON                      
         B     STIN30                                                           
*                                                                               
STIN20   DS    0H                                                               
         CLC   STAWORK(10),=CL10'LOCAL'                                         
         BNE   STIN40                                                           
         OC    OCNDPLR,OCNDPLR     LOCAL REGISTER  PENDING   ?                  
         BZ    STIN40              NO,   SKIP                                   
         LA    RF,OCNDPLR          DATE  OF   PENDING   LOCAL                   
*                                                                               
STIN30   DS    0H                                                               
         CLC   B4DAYCMP,0(RF)      DATE  BEFORE    B4DAYCMP  ?                  
         BL    *+8                 NO,   SKIP                                   
         MVI   DISPB4SW,YES        YES,  TURN ON   DISPLAY   BEFORE SW          
         MVI   0(R3),C'='          INSERT     EQUAL     SIGN                    
         AH    R0,=H'01'           ADD   ONE  TO   OFFSET                       
         LA    R3,1(,R3)           ADD   ONE  TO   NEXT AVAILABLE BYTE          
*                                                                               
         GOTO1 DATCON,PARMLIST,(2,0(RF)),(8,0(R3))                              
         AH    R0,=H'08'           ADD   8    TO   OFFSET                       
         LA    R3,8(,R3)           ADD   8    TO   NEXT AVAILABLE BYTE          
         STH   R0,STAOFF           SAVE  OFFSET                                 
*                                                                               
STIN40   DS    0H                                                               
*                                                                               
*        ADD   ANY ADDITIONAL SECONDARY FIELDS LOGIC IF APPLICABLE              
*                                                                               
         L     R1,SAVTADDR         RESTORE    TABLE     ENTRY   ADDRESS         
*                                                                               
*                                                                               
STIN80   DS    0H                                                               
*                                                                               
         B     XIT                 RETURN                                       
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        PRINTEL - PRINT AN  ELEMENT                                  *         
***********************************************************************         
*                                                                               
         USING HEADD,R3                                                         
         USING BIGPRNTD,R4                                                      
         USING BOXD,R6                                                          
PRINTEL  NTR1                                                                   
         L     R3,AHEAD            ->    HEADERS                                
         L     R4,VBIGPRNT         ->    BIGPRNTD                               
         L     R6,ADBXAREA         ->    BOXD                                   
*                                                                               
         TM    PRTSWS,PRTFIRST     FIRST PRINT ?                                
         BO    PRINT10             YES,  INITIALIZE     BOXES                   
         TM    PRTSWS,PRTNEWPG     NEW   PAGE NEEDED ?                          
         BO    PRINT30             YES,  GENERATE  NEW  PAGE                    
         CLC   LINE,DETMAX         ANY   MORE DETAIL    LINES     FIT ?         
         BNL   PRINT30             NO,   FORCE     NEW  PAGE                    
         B     PRINT60             YES,  PRINT                                  
*                                                                               
PRINT10  ZIC   R1,MAXLINES         MAX   LINES     PER  PAGE                    
         SH    R1,=H'02'           MINUS TWO                                    
         STC   R1,DETMAX           MAX   DETAIL    LINES                        
*                                  TURN  OFF  PRT  FIRST                        
         NI    PRTSWS,X'FF'-PRTFIRST                                            
*                                  TURN  OFF  PRT  NEW  PAGE                    
PRINT30  NI    PRTSWS,X'FF'-PRTNEWPG                                            
         AP    PAGENUM,=P'1'       NEW   PAGE NUMBER                            
         OI    PAGENUM+7,X'0F'                                                  
         UNPK  HDLPAGE#,PAGENUM+5(3)                                            
         MVI   LINE,INITLN         FORCE NEW  PAGE                              
         MVC   HDLACSID,ACCSYSID   INSERT     ACCPAK    SYTSTEM   ID            
         LA    R2,HDL1             ->    FIRST     HEAD LINE                    
         LA    R5,HDL#WOBX         PRINT HEAD LINES     W/O  BOXES              
*                                                                               
PRINT35  LA    RE,XP                                                            
         MVC   XP,SPACESX          CLEAR PRINT     LINES                        
         MVC   XPSECOND,SPACESX                                                 
         MVC   XPTHIRD,SPACESX                                                  
         MVC   XPFOURTH,SPACESX                                                 
*                                                                               
         LR    R1,R5               PRINT HEAD LINES     W/O  BOXES              
         CH    R1,=H'04'           PRINT MAX  OF   FOUR LINES                   
         BNH   *+8                                                              
         LA    R1,4                PRINT MAX  OF   FOUR LINES                   
*                                                                               
PRINT40  MVC   0(PGWDRL15,RE),0(R2)                                             
         CLC   0(STDPGWD,RE),SPACESX                                            
         BNE   *+8                 NO,   SKIP                                   
         MVI   0(RE),X'00'         YES,  FORCE     A    BLANK     LINE          
         LA    R2,PGWDRL15(,R2)    ->    NEXT                                   
         LA    RE,STDPGWD(,RE)     ->    NEXT                                   
         BCT   R1,PRINT40          KEEP  PRINTING                               
         GOTO1 REPORT              PRINT UP   TO   FOUR LINES                   
*                                                                               
         SH    R5,=H'04'           NUM   OF   HEAD LINES     LEFT               
         BP    PRINT35                                                          
*                                                                               
         MVC   XP,SPACESX          CLEAR PRINT     LINES                        
         MVC   XPSECOND,SPACESX                                                 
         MVC   XPTHIRD,SPACESX                                                  
         MVC   XPFOURTH,SPACESX                                                 
*                                                                               
         MVC   XPSECOND(PGWDRL15),HDLWB1                                        
         MVC   XPTHIRD(PGWDRL15),HDLWB2                                         
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS(STDPGWD),SPACESX                                         
*                                                                               
         ZIC   RF,LINE             INIT  BOXROWS                                
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         AH    RF,=H'03'                                                        
         MVI   0(RF),C'M'                                                       
*                                                                               
*                                  INIT  BOXCOLS                                
         L     RF,ABXCOLFM         ->    BOXCOLFM                               
         MVC   BOXCOLS(STDPGWD),0(RF)                                           
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,YES                                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
PRINT60  MVC   XP,SPACESX          CLEAR PRINT     LINES                        
         MVC   XPSECOND,SPACESX                                                 
         MVC   XPTHIRD,SPACESX                                                  
         MVC   XPFOURTH,SPACESX                                                 
*                                  INSERT     LINE                              
         L     R2,ADETAIL          ->    DETAIL    LINE                         
         LA    RE,XP               ->    OUTPUT    AREA                         
         MVC   0(PGWDRL15,RE),0(R2)                                             
*                                  IS    THE  LINE BLANK ?                      
         CLC   0(STDPGWD,RE),SPACESX                                            
         BNE   *+8                 NO,   SKIP                                   
         MVI   0(RE),X'00'         YES,  FORCE     A    BLANK     LINE          
         GOTO1 REPORT              PRINT THE  LINE                              
         MVC   XP,SPACESX          CLEAR PRINT     LINE                         
         B     XIT                 RETURN                                       
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*        OUTPUT AN ERROR MESSAGE                                      *         
***********************************************************************         
*                                                                               
ERR1     DS    0H                                                               
         L     R2,=A(MSG1)         ->    MSG  TEXT                              
         LA    R3,L'MSG1           LENGTH     OF   MESSAGE                      
         GOTO1 ERRMSG              OUTPUT     MESSAGE                           
         MVI   STOPSW,YES          SET   STOP SWITCH                            
         B     XIT                                                              
*                                                                               
         USING BIGPRNTD,R4                                                      
ERRMSG   NTR1                                                                   
         L     R4,VBIGPRNT         ->    BIGPRNTD                               
         LA    RE,XP                                                            
         MVC   XP,SPACESX          CLEAR PRINT     LINES                        
         MVC   XPSECOND,SPACESX                                                 
         MVC   XPTHIRD,SPACESX                                                  
         MVC   XPFOURTH,SPACESX                                                 
         BCTR  R3,0                MINUS ONE  FOR  EXECUTE                      
         EX    R3,MVCMSG           INSERT     TEXT                              
         GOTO1 REPORT              OUTPUT     THE  MESSAGE                      
         B     XIT                 EXIT                                         
*                                                                               
MVCMSG   MVC   XP+1(0),0(R2)       MOVE  MESSAGE                                
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*        DUMP  RECORDS OR ELEMENTS                                    *         
*                                                                     *         
*        R0    IDENTIFIES WHAT TO DUMP                                *         
***********************************************************************         
*                                                                               
DUMP     DS    0H                                                               
         CLI   DEBUGSW,YES         DEBUGGING  REQUESTED ?                       
         BNER  RE                  NO,   RETURN                                 
*                                                                               
         NTR1  ,                                                                
DUMP0    DS    0H                                                               
         CH    R0,=H'00'           DUMP  DKEY ?                                 
         BNE   DUMP1                                                            
         LA    R6,DKEY                                                          
         LA    R3,=C'DKY'                                                       
         LA    R4,L'DKEY                                                        
         B     DUMPIT                                                           
*                                                                               
DUMP1    DS    0H                                                               
         CH    R0,=H'01'           DUMP  DIR ?                                  
         BNE   DUMP2                                                            
         LA    R6,DIR                                                           
         LA    R3,=C'DIR'                                                       
         LA    R4,L'DIR                                                         
         B     DUMPIT                                                           
*                                                                               
DUMP2    DS    0H                                                               
         CH    R0,=H'02'           DUMP  AIO ?                                  
         BNE   DUMP3                                                            
         L     R6,AIO                                                           
         LA    R3,=C'AIO'                                                       
         LA    R4,300                                                           
         B     DUMPIT                                                           
*                                                                               
DUMP3    DS    0H                                                               
         CH    R0,=H'03'           DUMP  AI2 ?                                  
         BNE   DUMP4                                                            
         L     R6,AIO2                                                          
         LA    R3,=C'AI2'                                                       
         LA    R4,300                                                           
         B     DUMPIT                                                           
*                                                                               
DUMP4    DS    0H                                                               
         CH    R0,=H'04'           DUMP  OCN ?                                  
         BNE   DUMP5                                                            
         LR    R6,R4                                                            
         LA    R3,=C'OCN'                                                       
         LA    R4,OCNLN3Q                                                       
         B     DUMPIT                                                           
*                                                                               
DUMP5    DS    0H                                                               
         CH    R0,=H'05'           DUMP  CTFILE ?                               
         BNE   DUMP6                                                            
         L     R6,AIO3                                                          
         LA    R3,=C'CTF'                                                       
         LA    R4,200                                                           
         B     DUMPIT                                                           
*                                                                               
DUMP6    DS    0H                                                               
         CH    R0,=H'06'           DUMP  SKEY ?                                 
         BNE   DUMP7                                                            
         LA    R6,SKEY                                                          
         LA    R3,=C'SKY'                                                       
         LA    R4,L'SKEY                                                        
         B     DUMPIT                                                           
*                                                                               
DUMP7    DS    0H                                                               
         CH    R0,=H'07'           DUMP  AIOSL ?                                
         BNE   DUMP8                                                            
         L     R6,AIOSL                                                         
         LA    R3,=C'AIS'                                                       
         LA    R4,300                                                           
         B     DUMPIT                                                           
*                                                                               
DUMP8    DS    0H                                                               
         CH    R0,=H'8'            DUMP  DMCB ?                                 
         BNE   DUMP9                                                            
         LA    R6,DMCB                                                          
         LA    R3,=C'DMC'                                                       
         LA    R4,32                                                            
         B     DUMPIT                                                           
*                                                                               
DUMP9    DS    0H                                                               
         B     XIT                                                              
*                                                                               
DUMPIT   DS    0H                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 ,DUMPCB,(3,(R3)),(R6),C'DUMP',(R4),(R5),(C'P',PRINT)             
         GOTO1 PRNTBL                                                           
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EMAIL ERROR REPORT                                                 *          
**********************************************************************          
*                                                                               
SENDMAIL NTR1                                                                   
*MNFIL                                                                          
         CLI   MSTRT,X'FF'         SUPPRESS EMAILS RUN=TEST                     
         BE    XIT                 NO                                           
*MNFIL                                                                          
         CLI   MSTRT,0             IS THIS THE FIRST CALL?                      
         BNE   SENDM02             NO                                           
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         MVI   MSTRT,1                                                          
*                                                                               
SENDM02  CLI   MSTRT,1                                                          
         BNE   SENDM04                                                          
         GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO),(L'SUBDESC,SUBDESC)                
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEAD)                                    
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEADU)                                   
         MVI   MSTRT,2                                                          
*                                                                               
SENDM04  CLI   MSTRT,2                                                          
         BNE   SENDM06                                                          
         GOTOR VSMTP,DMCB,('SMTPAPTL',EDETAIL)                                  
         GOTOR VSMTP,DMCB,('SMTPAPTL',EDETAIL2)                                 
*                                                                               
SENDM06  CLI   MSTRT,3             FINISHED WITH THIS EMAIL?                    
         BNE   SENDMX              NO                                           
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         MVI   MSTRT,1             START AT HEADINGS                            
*                                                                               
SENDMX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
*                                                                               
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
ACCOUNT  DC    CL8'ACCOUNT '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
DMOPEN   DC    CL8'OPEN    '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
DMPCNT   DC    PL2'0'              DUMP  COUNT -   ERRORS                       
DMPMAX   DC    PL3'500'            MAX   DUMP COUNT                             
ZEROS    DC    PL8'0'                                                           
         DC    X'FF'                                                            
*                                                                               
SPACESX  DC    CL198' '            SPACES                                       
*                                                                               
HEXTABLE DC    C'0123456789ABCDEF'                                              
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
TOWHO    DC    C'NA-OOB_TEAM:'                                                  
*OWHO    DC    C'RDES:'                                                         
                                                                                
SUBDESC  DC    CL80'CHECK AUTHORIZATION FILE RECORD REPORT'                     
                                                                                
EHEAD    DS    0CL80                                                            
         DC    CL2'CO'                                                          
         DC    C' '                                                             
         DC    CL4'FILE'                                                        
         DC    C' '                                                             
         DC    CL7'AGENCY '                                                     
         DC    C' '                                                             
         DC    CL2'UL'                                                          
         DC    C' '                                                             
         DC    CL6'STRT #'                                                      
         DC    C' '                                                             
         DC    CL6'NEXT #'                                                      
         DC    C' '                                                             
         DC    CL12'BANK ACCOUNT'                                               
         DC    C' '                                                             
         DC    CL3'OFC'                                                         
         DC    C' '                                                             
         DC    CL6'LCHECK'                                                      
         DC    C' '                                                             
         DC    CL8'SORT SEQ'                                                    
         DC    C' '                                                             
         DC    CL2'PO'                                                          
         DS    CL(L'EHEAD-(*-EHEAD))' '                                         
*                                                                               
EHEADU   DS    0CL80                                                            
         DC    CL2'__'                                                          
         DC    C' '                                                             
         DC    CL4'____'                                                        
         DC    C' '                                                             
         DC    CL7'_______'                                                     
         DC    C' '                                                             
         DC    CL2'__'                                                          
         DC    C' '                                                             
         DC    CL6'______'                                                      
         DC    C' '                                                             
         DC    CL6'______'                                                      
         DC    C' '                                                             
         DC    CL12'____________'                                               
         DC    C' '                                                             
         DC    CL3'___'                                                         
         DC    C' '                                                             
         DC    CL6'______'                                                      
         DC    C' '                                                             
         DC    CL8'________'                                                    
         DC    C' '                                                             
         DC    CL2'__'                                                          
         DS    CL(L'EHEADU-(*-EHEADU))' '                                       
         EJECT                                                                  
***********************************************************************         
*        ASSEMBLER/LINKAGE EDITOR RELOCATABLE CONSTANTS               *         
***********************************************************************         
         SPACE 1                                                                
CENTER   DC    V(CENTER)           CENTER                                       
PRNTBL   DC    V(PRNTBL)           PRNTBL                                       
*                                                                               
REPORT   DC    A(REPORTER)                                                      
AGETNUM  DC    A(GETNUM)           GETNUM                                       
AIO      DC    A(IO)               ADDR  I/O  AREA FOR  CHECK   RECORD          
AIO2     DC    A(IO2)              ADDR  I/O  AREA FOR  COMPANY RECORD          
AIO3     DC    A(IO3)              ADDR  I/O  AREA FOR  ID      RECORD          
AIOSL    DC    A(IOSL)             ADDR  I/O  AREA FOR  SYSTEM LIST REC         
AHEAD    DC    A(HEAD)                                                          
ADETAIL  DC    A(DETAIL)                                                        
ASTALST  DC    A(STATLST)                                                       
ASTALST2 DC    A(STATLST2)                                                      
ASTALASR DC    A(STATLASR)                                                      
ASORTS   DC    A(SORTS)                                                         
ABXCOLFM DC    A(BOXCOLFM)                                                      
         EJECT                                                                  
***********************************************************************         
*        LITERALS                                                     *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         DROP  R8,R9,RB                                                         
         EJECT ,                                                                
***********************************************************************         
*        MESSAGES                                                     *         
***********************************************************************         
*                                                                               
MSG1     DC    C'INVALID QSELECT'                                               
         EJECT ,                                                                
***********************************************************************         
*        TABLES                                                       *         
***********************************************************************         
***********************************************************************         
*        STATLST - STATUS BIT TABLES                                  *         
*                                                                     *         
*                BYTE 1     - 00 - NO   SECONDARY DATA                *         
*                           - 80 - SECONDARY      DATA                *         
*                           - FF - END  OF        TABLE               *         
*                BYTE  2    - BIT  SETTING                            *         
*                BYTES 3-12 - LEFT SIDE IF ENTRY                      *         
***********************************************************************         
*                                                                               
STATLST  DS    0CL12                                                            
         DC    X'00',AL1(OCNSPRTQ),CL10'DIRECT'                                 
         DC    X'00',AL1(OCNSOFFR),CL10'OFFICE'                                 
         DC    X'00',AL1(OCNSPRTN),CL10'NUMBER'                                 
         DC    X'00',AL1(OCNSDREG),CL10'DIREG'                                  
         DC    X'00',AL1(OCNSAUTH),CL10'AUTH'                                   
         DC    X'00',AL1(OCNSSHUT),CL10'SHUTTLE'                                
         DC    X'80',AL1(OCNSSOON),CL10'SOON'                                   
         DC    X'80',AL1(OCNSLOCL),CL10'LOCAL'                                  
         DC    X'00',AL1(0),CL10'STACK'                                         
         DC    X'FF'                                                            
*                                                                               
STATLST2 DS    0CL12                                                            
         DC    X'00',AL1(OCNSMICR),CL10'MICR'                                   
         DC    X'00',AL1(OCNSFTP),CL10'FTP'                                     
         DC    X'00',AL1(OCNSLBLT),CL10'LASER'   BOTTOM LINE TECHNOLOGY         
         DC    X'00',AL1(OCNS820),CL10'EDI820'                                  
         DC    X'00',AL1(OCNSEFT),CL10'EFT'                                     
         DC    X'00',AL1(OCNSDFIL),CL10'FLAT FILE'                              
         DC    X'FF'                                                            
*                                                                               
STATLASR DS    0CL12                                                            
         DC    X'00',AL1(OCNRED),CL10'LASERRED'                                 
         DC    X'00',AL1(OCNBLU),CL10'LASERBLU'                                 
         DC    X'00',AL1(OCNGRN),CL10'LASERGRE'                                 
         DC    X'00',AL1(OCNGLD),CL10'LASERGLD'                                 
         DC    X'00',AL1(OCNBRN),CL10'LASERBRN'                                 
         DC    X'00',AL1(OCNVIO),CL10'LASERVIO'                                 
         DC    X'00',AL1(OCNWSP),CL10'LASERWSP'                                 
         DC    X'FF'                                                            
*                                                                               
SORTS    DS    0CL9                                                             
         DC    C'A',CL8'AMOUNT'                                                 
         DC    C'C',CL8'CODE'                                                   
         DC    C'D',CL8'DISCOUNT'                                               
         DC    C'N',CL8'NAME'                                                   
         DC    X'FF'                                                            
*                                                                               
         EJECT ,                                                                
BOXCOLFM DC    CL(STDPGWD)' '      CLEAR BOX  COL  FORMAT                       
         ORG   BOXCOLFM+HDLIDXX-HDLWB2-1      FIRST     COLUMN                  
         DC    CL1'L'                                                           
         ORG   BOXCOLFM+HDLLDGRX-HDLWB2-1     NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+HDLCHCK1-HDLWB2-1     NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+HDLCHCK2-HDLWB2-1     NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+HDLACNTX-HDLWB2-1     NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+HDLFILTX-HDLWB2-1     NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+HDLCHCK3-HDLWB2-1     NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+HDLSEQX-HDLWB2-1      NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+HDLOFFX-HDLWB2-1      NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+HDLSTATX-HDLWB2-1     NEXT      COLUMN                  
         DC    CL1'C'                                                           
         ORG   BOXCOLFM+PGWDRL15-1            LAST      COLUMN                  
         DC    CL1'R'                                                           
         ORG   ,                                                                
*                                                                               
         TITLE 'BOX HOOK --> CONTROL PRINTING OF BOXES'                         
***********************************************************************         
*        BOX   HOOK                                                   *         
***********************************************************************         
*                                                                               
         USING BIGPRNTD,R4                                                      
         USING BOXD,R6                                                          
         USING ACCZD,RA                                                         
         USING ACWORKD,RC                                                       
         SPACE 1                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BXHK*                                                         
         L     RC,CZBASEC                                                       
         L     R4,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
         MVI   BOXWT,1             INITIALIZE                                   
         MVC   BOXROWS,SPACES      CLEAR BOXES                                  
         MVC   BOXCOLS,XSPACES     CLEAR BOXES                                  
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVC   BOXSHADE,PRTOPTS2                                                
         LA    R1,STDPGWD                                                       
         ST    R1,BOXWIDTH                                                      
         MVC   BOXWIDTH,PAGEWDTH                                                
         NI    BOXSHADE,PRTSTRIP+PRTSHADI+PRTSHADO                              
         MVI   BOXSHCH1,X'42'      SHADE CHARACTER                              
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6,RA,RB,RC                                                   
         TITLE 'GET NUMERIC INPUT'                                              
***********************************************************************         
*        GET NUMERIC INPUT - CONVERT FROM INPUT FORMAT TO BINARY      *         
*                                                                     *         
*        INPUT:                                                       *         
*          R1 = ADDRESS OF THE PARAMETER LIST                         *         
*                                                                     *         
*          INPUT  PARMLIST:                                           *         
*             PARM1:                                                  *         
*               BYTE  0   LENGTH  OF THE FIELD <= 11 (MAY BE SIGNED)  *         
*               BYTES 1-3 ADDRESS OF THE FIELD                        *         
*                                                                     *         
*          OUTPUT PARMLIST:                                           *         
*             PARM1:                                                  *         
*               BYTE  0   ERROR  INDICATOR                            *         
*                          0 = GOOD VALUE                             *         
*                          4 = OVERFLOW                               *         
*                          8 = NOT  NUMERIC                           *         
*                         16 = BAD  PARAMETER                         *         
*             PARM2:                                                  *         
*               BYTES 0-3 BINARY VALUE OF INPUT FIELD                 *         
***********************************************************************         
*                                                                               
         USING ACCZD,RA                                                         
         USING ACWORKD,RC                                                       
         SPACE 1                                                                
GETNUM   NMOD1 0,*GETN*                                                         
         L     RC,CZBASEC                                                       
         CLI   0(R1),GETNMAXD      INPUT SIZE >    11                           
         BH    GETNUMER            INVALID    INPUT                             
         L     R2,0(,R1)           ->    THE  FIELD                             
         LA    R2,0(,R2)           REMOVE     HIGH ORDER     BYTE               
         ZIC   R3,0(,R1)           GET   FIELD     LENGTH                       
         AR    R2,R3               FIND  LAST BYTE                              
         BCTR  R2,0                                                             
         LA    R5,WORK+GETNMXM1    ->    WORK AREA (11  DIGITS    MAX)          
*                                  INITIALIZE WORK AREA                         
         MVC   WORK(GETNMAXD),CZEROS                                            
         MVI   BYTE,X'00'          CLEAR SWITCHES                               
*                                                                               
GETNUM10 CLI   0(R2),SPACE         SPACE ?                                      
         BE    GETNUM20            YES,  CHECK     STARTED                      
         CLI   0(R2),C'+'          PLUS  SIGN ?                                 
         BE    GETNUM30            YES,  VALIDATE  SIGN                         
         CLI   0(R2),C'-'          MINUS SIGN ?                                 
         BE    GETNUM30            YES,  VALIDATE  SIGN                         
         TM    BYTE,GETNEND        FIELD ENDED ?                                
         BO    GETNUMNG            YES,  NOT  NUMERIC                           
         CLI   0(R2),C'0'          IS    IT   NUMERIC ?                         
         BL    GETNUMNG                                                         
         CLI   0(R2),C'9'                                                       
         BH    GETNUMNG                                                         
*                                  NUMERIC                                      
         OI    BYTE,GETNDIG        SAY   FOUND     DIGIT                        
         MVC   0(1,R5),0(R2)       SAVE  THIS BYTE                              
         BCTR  R5,0                PREV  SAVE BYTE                              
         B     GETNUM40            GET   PREV CHARACTER                         
*                                                                               
GETNUM20 TM    BYTE,GETNDIG        ANY   DIGITS    FOUND ?                      
         BZ    GETNUM25            NO,   SKIP                                   
         OI    BYTE,GETNEND        YES,  FIELD     ENDED                        
         B     GETNUM40            GET   PREV CHARACTER                         
*                                                                               
GETNUM25 TM    BYTE,GETNSIGN       ANY   SIGN FOUND ?                           
         BO    GETNUMNG            YES,  NOT  NUMERIC                           
         B     GETNUM40            GET   PREV CHARACTER                         
*                                                                               
GETNUM30 TM    BYTE,GETNEND+GETNSIGN     OR   SIGN      FOUND ?                 
         BNZ   GETNUMNG            YES,  NOT  NUMERIC                           
         OI    BYTE,GETNSIGN       TURN  ON   FOUND     SIGN                    
         TM    BYTE,GETNDIG        DIGIT FOUND ?                                
         BZ    *+8                 NO,   SKIP                                   
         OI    BYTE,GETNEND        TURN  ON   END  OF   INPUT                   
         CLI   0(R2),C'-'          MINUS SIGN ?                                 
         BNE   *+8                 NO,   SKIP                                   
         OI    BYTE,GETNNEG        TURN  ON   MINUS                             
*                                                                               
GETNUM40 BCTR  R2,0                BUMP  TO   PREVIOUS                          
         BCT   R3,GETNUM10         TEST  PREV CHARACTER                         
*                                                                               
         TM    BYTE,GETNDIG        DIGIT FOUND ?                                
         BZ    GETNUMNG            NO,   NOT  NUMERIC                           
         NI    WORK+GETNMXM1,X'CF' TURN  OFF  BITS 2,3 (X'CN')                  
         TM    BYTE,GETNNEG        MINUS SIGN FOUND ?                           
         BZ    *+8                 NO,   SKIP                                   
         OI    WORK+GETNMXM1,X'10' TURN  ON   MINUS    (X'DN')                  
         PACK  PCK,WORK(GETNMAXD)  PACK  THE  NUMBER                            
         CP    PCK,=P'2147483647'  MAX   PACKED    NUMBER                       
         BH    GETNUMOV            HIGH, OVERFLOW                               
         CP    PCK,=P'-2147483648' MIN   PACKED    NUMBER                       
         BL    GETNUMOV            LOW,  OVERFLOW                               
         CVB   R2,PCK              CONVERT    TO   BINARY                       
         ST    R2,4(,R1)           SAVE  THE  NUMBER                            
         SR    RF,RF               GOOD  RETURN    CODE                         
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMOV LA    RF,4                OVERFLOW ERROR                               
         B     GETNUMEX                                                         
*                                                                               
GETNUMNG LA    RF,8                NOT NUMERIC ERROR                            
         B     GETNUMEX            RETURN                                       
*                                                                               
GETNUMER LA    RF,16               OVERFLOW ERROR                               
*                                                                               
GETNUMEX STC   RF,0(,R1)           STORE RETURN    CODE                         
         XMOD1 ,                   EXIT                                         
         DROP  RA,RC                                                            
         EJECT ,                                                                
*                                                                               
*        CONSTANTS FOR GETNUM                                                   
*                                                                               
CZEROS   DC    16CL1'0'            ZEROES                                       
         SPACE 3                                                                
*                                                                               
*        EQUATES FOR BITS IN BYTE                                               
*                                                                               
GETNDIG  EQU   X'80'               DIGIT FOUND                                  
GETNEND  EQU   X'40'               END   OF   VALUE     FOUND                   
GETNSIGN EQU   X'08'               SIGN  FOUND                                  
GETNNEG  EQU   X'04'               MINUS FOUND                                  
*                                                                               
GETNMAXD EQU   11                  MAX   NUM  OF   DIGITS                       
GETNMXM1 EQU   GETNMAXD-1          MAX   NUM  OF   DIGS MINUS 1                 
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'ACREPORT HOOK --> CONTROL PRINTING OF REPORT'                   
***********************************************************************         
*        ACREPORT HOOK                                                *         
***********************************************************************         
*                                                                               
         USING BIGPRNTD,R4                                                      
         USING BOXD,R6                                                          
         USING ACCZD,RA                                                         
         USING ACWORKD,RC                                                       
         SPACE 1                                                                
REPORTER DS    0D                                                               
         NMOD1 0,*RPHK*                                                         
         L     RC,CZBASEC                                                       
         L     R4,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
         LA    R1,STDPGWD          SET   TO   198  EACH TIME IN                 
         ST    R1,BOXWIDTH                                                      
         GOTO1 ACREPORT                                                         
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         DROP  R4,R6,RA,RB,RC                                                   
         TITLE 'CHECK AUTHORIZATION FILE RECORD REPORT PROGRAM'                 
***********************************************************************         
*        BUFFERS                                                      *         
***********************************************************************         
*                                                                               
         DC    F'0'                IOAREA     #1 - CHECK     RECORD             
IO       DC    (MXRLNQ)X'00'                                                    
         DC    F'0'                IOAREA     #2 - COMPANY   RECORD             
IO2      DC    (MXRLNQ)X'00'                                                    
         DC    F'0'                IOAREA     #3 - ID        RECORD             
IO3      DC    (MXRLNQ)X'00'                                                    
         DC    F'0'                IOAREA     #4 - SYSTEM    LIST REC           
IOSL     DC    (MXRLNQ)X'00'                                                    
         EJECT                                                                  
***********************************************************************         
*        HEADERS AND DETAIL                                           *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
HEAD     DC    (HDLLEN)C' '        HEADERS                                      
*                                                                               
         DS    0D                                                               
DETAIL   DC    (DETLEN)C' '        DETAIL                                       
         EJECT                                                                  
***********************************************************************         
*        EQUATES                                                      *         
***********************************************************************         
*                                                                               
MAXLEN   EQU   12                  MAX   LENGTH    OF   ACCOUNT                 
MXRLNQ   EQU   2000                MAX   LENGTH    OF   RECORD                  
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
*                                                                               
PGWDRL10 EQU   110                 WIDTH OF   PAGE FOR 10X8 AT PITCH 10         
PGWDRL12 EQU   132                 WIDTH OF   PAGE FOR 10X8 AT PITCH 12         
PGWDRL15 EQU   164                 WIDTH OF   PAGE FOR 10X8 AT PITCH 15         
*                                                                               
STDPGWD  EQU   L'XHEAD1            STANDARD   PAGE WIDTH                        
*                                                                               
SPACE    EQU   X'40'                                                            
*                                                                               
INITLN   EQU   99                  MAX   LINE,   FORCE     NEW  PAGE            
SHORTPG  EQU   60                  LASER PAPER,  10X8                           
LONGPG   EQU   66                  PC    PAPER,  8X10                           
         EJECT                                                                  
***********************************************************************         
*        DSECT FOR STORAGE AREA                                       *         
***********************************************************************         
*                                                                               
ACCZD    DSECT                                                                  
RELO     DS    F                   A(0) OR A(MAIN PROGRAM)                      
*                                                                               
CZBASEA  DS    F                   RA BASE                                      
CZBASEB  DS    F                   RB BASE                                      
CZBASEC  DS    F                   RC BASE                                      
*                                                                               
AUTL     DS    A                   UTL                                          
*                                                                               
SAVTADDR DS    A                   SAVE TABLE ADDRESS                           
*                                                                               
PARMLIST DS    6F                  PARM LIST                                    
DUMPCB   DS    6F                  DUMP PARM LIST                               
*                                                                               
SKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY FOR SYS LIST RCD               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DKEYSV   DS    CL(L'ACCKEY)        DIRECTORY KEY SAVED                          
DIR      DS    CL64                DIRECTORY RECORD                             
DIRSV    DS    CL(L'DIR)           DIRECTORY RECORD SAVED                       
DA       DS    F                                                                
*                                                                               
PCK      DS    D                   WORK AREA PACKED                             
*                                                                               
WORKID   DS    CL7                 WORK ID                                      
*                                                                               
COMPANY  DS    XL1                 COMPANY CODE                                 
SAVEUNLD DS    CL2                 SAVE INIT/LEDGER                             
*                                                                               
CTSAVE   DS    CL(L'CTIKEY)                                                     
*                                                                               
         DS    0D                  ALLIGN ON DOUBLEWORD                         
STAWORK  DS    CL16                STATUS WORK AREA                             
STAOFF   DS    H                   STATUS OFFSET                                
STABYTE  DS    CL1                 STATUS WORK BYTE                             
*                                                                               
READSEQ  DS    X                   READ DIRECTORY SEQUENTIALLY                  
READSSEQ EQU   X'80'               READ SYSTEM LIST SEQUENTIALLY                
READ0SEQ EQU   X'40'               READ CHECK REC SEQUENTIALLY                  
*                                                                               
PRTOPTS1 DS    X                   PRINT OPTIONS 1                              
PRTBOXES EQU   X'80'                     PRINT BOXES IF ON                      
*        EQU   X'40'                     RESERVED                               
*        EQU   X'20'                     RESERVED                               
*        EQU   X'10'                     RESERVED                               
*        EQU   X'08'                     RESERVED                               
*        EQU   X'04'                     RESERVED                               
*        EQU   X'02'                     RESERVED                               
*        EQU   X'01'                     PRINT MIXED CASE                       
*                                                                               
PRTOPTS2 DS    X                   PRINT OPTIONS 2                              
*        EQU   X'80'                     RESERVED                               
*        EQU   X'40'                     RESERVED                               
*        EQU   X'20'                     RESERVED                               
*        EQU   X'10'                     RESERVED                               
*        EQU   X'08'                     RESERVED                               
PRTSTRIP EQU   X'04'                     PRINT STRIPES                          
PRTSHADO EQU   X'02'                     PRINT SHADE OUTSIDE BOX                
PRTSHADI EQU   X'01'                     PRINT SHADE INSIDE BOX                 
*                                                                               
PAGENUM  DS    PL8                 PAGE NUMBER                                  
DETMAX   DS    AL1                 MAX DETAIL LINES PER PAGE                    
*                                                                               
PAGEWDTH DS    A                   PAGE WIDTH (LEAVE AS ADDR TYPE)              
*                                                                               
PRTSWS   DS    X                   PRINT SWITCHES                               
PRTNEWPG EQU   X'80'                     NEW PAGE                               
PRTFIRST EQU   X'40'                     1ST PRINT                              
*                                                                               
SAVESE   DS    X                   SE NUM FOR CTL FILE                          
SYSSE    DS    X                   SE NUM FOR ACCPAK                            
*MNFIL ACCSYSID DS    CL4                 ACCPAK SYSTEM ID                      
ACCSYSID DS    CL5                 ACCPAK SYSTEM ID                             
*MNFIL                                                                          
*                                                                               
DISPB4#  DS    F                   DISPLAY BEFORE NUM OF DAYS                   
TODAYBCD DS    CL6                 TODAY'S DATE EBCDIC YYMMDD                   
B4DAYBCD DS    CL6                 BEFORE DATE EBCDIC YYMMDD                    
B4DAYCMP DS    XL2                 BEFORE DATE COMPRESSED                       
DISPB4SW DS    XL1                 DATE BEFORE DISPLAY DATE FOUND               
*                                                                               
UPSI     DS    X                   UPSI SWITCH                                  
DEBUGSW  DS    X                   DEBUG SWITCH                                 
STOPSW   DS    X                   STOP RUN INDICATOR                           
*                                                                               
MSTRT    DS    X                   EMAIL STAGE                                  
*                                                                               
*                                  EMAIL DETAIL LINE                            
EDETAIL  DS    0CL80                                                            
EDCPY    DS    CL2                 COMPANY CODE                                 
         DS    CL1                                                              
*MNFIL                                                                          
*EDACC    DS    CL4                 ACCSYS                                      
EDACC    DS    CL5                 ACCSYS                                       
*MNFIL                                                                          
         DS    CL1                                                              
EDID     DS    CL7                 ID                                           
         DS    CL1                                                              
EDUL     DS    CL2                 UNIT/LEDGER                                  
         DS    CL1                                                              
EDSTRT   DS    CL6                 STARTING CHECK NUMBER                        
         DS    CL1                                                              
EDNEXT   DS    CL6                 NEXT CHECK NUMBER                            
         DS    CL1                                                              
EDBANK   DS    CL12                BANK ACCOUNT                                 
         DS    CL1                                                              
EDFILT   DS    CL3                 OFFICE/CLIENT FILTER                         
         DS    CL1                                                              
EDLCHK   DS    CL6                 LAST CHECK NUMBER                            
         DS    CL1                                                              
EDSORT   DS    CL8                 SORT SEQUENCE                                
         DS    CL1                                                              
EDPOFF   DS    CL2                 POSTING OFFICE                               
         DS    CL(L'EDETAIL-(*-EDETAIL))                                        
*                                                                               
EDETAIL2 DS    0CL80                                                            
         DS    CL4                                                              
EDSTAT   DS    CL76                CURRENT STATUS                               
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER OUR REPORT HEADERS                                   *         
***********************************************************************         
*                                                                               
HEADD    DSECT                                                                  
HDL1     DS    0CL(PGWDRL15)       HEADING LINE 1 (164 BYTES)                   
         DS    CL64                                                             
HDLCPY   DS    CL36                COMPANY NAME                                 
         DS    CL64                                                             
*                                                                               
HDL2     DS    0CL(PGWDRL15)       HEADING LINE 2                               
         DS    CL1                                                              
HDLCPYCX DS    CL8                 COMPANY:                                     
         DS    CL1                                                              
HDLCPYCD DS    CL2                 COMPANY CODE                                 
         DS    CL130                                                            
HDLPAGEX DS    CL5                 PAGE:                                        
         DS    CL3                                                              
HDLPAGE# DS    CL5                 PAGE NUMBER                                  
         DS    CL9                                                              
*                                                                               
HDL3     DS    0CL(PGWDRL15)       HEADING LINE 3                               
         DS    CL1                                                              
HDLACSYS DS    CL8                 ACC SYS:                                     
         DS    CL1                                                              
HDLACSID DS    CL5                 ACC SYS ID:                                  
         DS    CL149                                                            
*                                                                               
HDL4     DS    1CL(PGWDRL15)       HEADING LINE 4                               
*                                                                               
HDL#WOBX EQU   (*-HEADD)/PGWDRL15  NUM OF UNBOXED HEADING LINES                 
*                                                                               
HDLWB1   DS    0CL(PGWDRL15)       HEADING LINE WITH BOXES 1                    
         DS    CL1                                                              
HDLIDX   DS    CL7                 ID                                           
         DS    CL1                                                              
HDLUNITX DS    CL6                 UNIT                                         
         DS    CL1                                                              
HDLSTRTX DS    CL6                 STARTING                                     
         DS    CL1                                                              
HDLNEXTX DS    CL6                 NEXT                                         
         DS    CL1                                                              
HDLBANKX DS    CL12                BANK                                         
         DS    CL1                                                              
HDLOFCLX DS    CL13                OFFICE/CLIENT                                
         DS    CL1                                                              
HDLLASTX DS    CL6                 LAST                                         
         DS    CL1                                                              
HDLSORTX DS    CL8                 SORT                                         
         DS    CL1                                                              
HDLPOSTX DS    CL7                 POSTING                                      
         DS    CL1                                                              
         DS    CL89                CURRENT STATUS (BLANK)                       
         DS    CL1                                                              
*                                                                               
HDLWB2   DS    0CL(PGWDRL15)       HEADING LINE WITH BOXES 2                    
         DS    CL1                                                              
HDLIDXX  DS    CL7                 --                                           
         DS    CL1                                                              
HDLLDGRX DS    CL6                 LEDGER                                       
         DS    CL1                                                              
HDLCHCK1 DS    CL6                 CHECK                                        
         DS    CL1                                                              
HDLCHCK2 DS    CL6                 CHECK                                        
         DS    CL1                                                              
HDLACNTX DS    CL12                ACCOUNT                                      
         DS    CL1                                                              
HDLFILTX DS    CL13                FILTER                                       
         DS    CL1                                                              
HDLCHCK3 DS    CL6                 CHECK                                        
         DS    CL1                                                              
HDLSEQX  DS    CL8                 SEQUENCE                                     
         DS    CL1                                                              
HDLOFFX  DS    CL7                 OFFICE                                       
         DS    CL1                                                              
HDLSTATX DS    CL89                CURRENT STATUS                               
         DS    CL1                                                              
*                                                                               
HDLLEN   EQU   *-HEADD             LENGTH OF HEADING AREA                       
HDLLINES EQU   HDLLEN/PGWDRL15                                                  
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER OUR REPORT DETAIL LINE                               *         
***********************************************************************         
*                                                                               
DETAILD  DSECT                                                                  
         DS    CL1                                                              
DETID    DS    CL7                 ID                                           
         DS    CL1                                                              
         DS    CL2                                                              
DETUNLD  DS    CL2                 UNIT/LEDGER                                  
         DS    CL2                                                              
         DS    CL1                                                              
DETSCHK  DS    CL6                 STARTING CHECK NUMBER                        
         DS    CL1                                                              
DETNCHK  DS    CL6                 NEXT CHECK NUMBER                            
         DS    CL1                                                              
DETBANK  DS    CL12                BANK ACCOUNT                                 
         DS    CL1                                                              
         DS    CL5                                                              
DETFILT  DS    CL3                 OFFICE/CLIENT FILTER                         
         DS    CL5                                                              
         DS    CL1                                                              
DETLCHK  DS    CL6                 LAST CHECK NUMBER                            
         DS    CL1                                                              
DETSORT  DS    CL8                 SORT SEQUENCE                                
         DS    CL1                                                              
         DS    CL2                                                              
DETPOFF  DS    CL2                 POSTING OFFICE                               
         DS    CL3                                                              
         DS    CL1                                                              
DETSTAT  DS    CL89                CURRENT STATUS                               
         DS    CL1                                                              
*                                                                               
DETLEN   EQU   *-DETAILD           LENGTH OF HEADING AREA                       
DETLINES EQU   DETLEN/PGWDRL15     SHOULD BE ONE                                
         EJECT                                                                  
         EJECT ,                                                                
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT OFF                                                              
*ACMASTD                                                                        
         PRINT OFF                                                              
**********NCLUDE ACMASTD                                                        
         PRINT OFF                                                              
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*ACBIGPRNTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
*DDBOXEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDSMTPD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREPCZ02 12/16/15'                                      
         END                                                                    
