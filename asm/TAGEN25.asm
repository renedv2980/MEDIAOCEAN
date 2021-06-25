*          DATA SET TAGEN25    AT LEVEL 047 AS OF 02/05/13                      
*PHASE T70225A,*                                                                
         TITLE 'T70225 - PRODUCTION INTERFACE MAINTENANCE'                      
T70225   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70225,R7                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
         CLI   THISLSEL,C'D'       IF DELETING FROM LIST                        
         BE    INT10                                                            
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    INT30                                                            
*                                                                               
INT10    CLI   MODE,DISPKEY        IF MODE IS DISPLAY KEY                       
         BE    DK                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    INT60                                                            
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    INT60                                                            
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BE    INT20                                                            
         CLI   MODE,XRECREST       RECORD RESTORED                              
         BE    INT20                                                            
         CLI   MODE,XRECADD        NEW RECORD ADDED                             
         BE    INT20                                                            
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         BNE   INT50                                                            
*                                                                               
INT20    GOTO1 ADDPTRS,DMCB,PTRBLK                                              
*                                                                               
INT30    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     INTX                                                             
*                                                                               
INT50    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   INTX                                                             
         BAS   RE,BLDREC                                                        
         B     INTX                                                             
*                                                                               
INT60    XC    PTRBLK,PTRBLK       CLEAR POINTER BLOCK                          
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
*                                                                               
INTX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VK       DS    0H                                                               
         MVC   SIFAGYN,SPACES      CLEAR NAMES FROM SCREEN                      
         OI    SIFAGYNH+6,X'80'                                                 
         MVC   SIFCLIN,SPACES                                                   
         OI    SIFCLINH+6,X'80'                                                 
*                                                                               
VK5      GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SIFAGYH),SIFAGYNH                     
*                                                                               
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVAGSTA6,TAAYSTA6   SAVE AGENCY STATUS 6                         
         DROP  R4                                                               
*                                                                               
         LA    R2,SIFCLIH                                                       
         CLI   5(R2),0             IF ANY INPUT - VALIDATE IT                   
         BNE   VK10                                                             
         XC    SIFCLI,SIFCLI       CLEAR PREVIOUS INPUT - IF ANY                
         XC    TGPCLI,TGPCLI                                                    
         MVC   SIFCLIN,SPACES                                                   
         OI    SIFCLIH+6,X'80'     AND TRANSMIT                                 
         OI    SIFCLINH+6,X'80'                                                 
         TM    SCRSTAT,SCRCHG      IF 1ST TIME ON SCREEN                        
         BNO   VK30                                                             
         GOTO1 RECVAL,DMCB,TLIFCDQ,(X'04',0)                                    
         BNE   VK30                THEN CHECK IF REC HAS CLIENT                 
         LA    R4,KEY                                                           
         USING TLIFD,R4                                                         
         OC    TLIFPCLI,TLIFPCLI   IF NOT - EXIT                                
         BZ    VKX                                                              
*                                  PRODUCTION CLIENT INPUT                      
VK10     CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BNE   *+12                                                             
         BAS   RE,RDIFAGY          MAKE SURE RECORD EXISTS AT AGY LEVEL         
         B     VK20                                                             
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BE    VK20                                                             
         CLI   THISLSEL,C'C'       OR IF CHANGING FROM LIST                     
         BNE   VK25                                                             
*                                                                               
VK20     OC    SIFALPH,SPACES      MUST VALIDATE ALPHA ID 1ST                   
         LA    R2,SIFALPHH                                                      
         CLI   5(R2),0             TO GET HEX                                   
         BE    MISSERR                                                          
         MVC   AIO,AIO3                                                         
         GOTO1 USERVAL,DMCB,(X'40',SIFALPHH),SIFALPNH                           
         MVC   AIO,AIO1                                                         
         CLI   TGACCHX,0           IF NO HEX COMP                               
         BE    *+8                 DON'T SET FIELD VALID                        
         OI    4(R2),X'20'         FIELD VALIDATED                              
         BAS   RE,RDPCLI           VALIDATE PRODUCTION CLIENT                   
*                                                                               
VK25     MVC   TGPCLI(3),SIFCLI    SET GLOBAL PRODUCTION CLIENT                 
         OC    TGPCLI,SPACES                                                    
         B     VK40                                                             
*                                                                               
VK30     XC    TGPCLI,TGPCLI       CLEAR GLOBAL PROD CLIENT                     
VK40     GOTO1 RECVAL,DMCB,TLIFCDQ,(X'40',0)                                    
VKX      B     XIT                                                              
         EJECT                                                                  
*              ADDING AT THE PRODUCTION CLIENT LEVEL - MUST MAKE                
*              SURE INTERFACE RECORD EXISTS AT THE AGENCY LEVEL                 
         SPACE 1                                                                
RDIFAGY  NTR1                                                                   
         XC    TGPCLI,TGPCLI       CLEAR PRODUCTION CLIENT                      
         GOTO1 RECVAL,DMCB,TLIFCDQ,0                                            
         BNE   ERRLEVEL            AGENCY LEVEL MUST EXIST                      
         B     XIT                                                              
         SPACE 3                                                                
*        VALIDATE PRODUCTION CLIENT                                             
*                                                                               
RDPCLI   NTR1                                                                   
         LA    R2,SIFCLIH          PRODUCTION CLIENT                            
         OC    8(3,R2),SPACES                                                   
         MVC   AIO,AIO3                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),TGACCHX      COMPANY                                      
         MVC   KEY+1(2),=C'SJ'     U/L                                          
         MVC   KEY+3(3),8(R2)      PRODUCTION CLIENT CODE                       
         GOTO1 READACC,DMCB,0      READ ACC FILE                                
         MVC   AIO,AIO1                                                         
         BNE   ERRXIT              ERROR IS SET IN READACC                      
         MVC   TGPCLI(3),8(R2)                                                  
         OC    TGPCLI,SPACES       PAD WITH SPACES                              
         BAS   RE,GETLONG                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        GET LONG NAME FROM ACC RECORD                                          
*                                                                               
GETLONG  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         L     R4,AIO                                                           
         LA    R2,SIFCLINH         POINT TO CLIENT NAME                         
         LA    R4,A$ACCORFST(R4)   POINT TO 1ST ELEMENT OLD FILE                
*                                                                               
GETL10   CLI   0(R4),0             END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'20'         IF THIS NAME ELEMENT                         
         BE    GETL20              GET NAME                                     
         SR    R1,R1                                                            
         IC    R1,1(R4)            BUMP TO NEXT ELEMENT                         
         LTR   R1,R1               SET CC                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R1                                                            
         B     GETL10                                                           
*                                                                               
         USING ACNAMED,R4                                                       
GETL20   MVC   8(36,R2),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ACNMNAME                                                 
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         MVC   AIO,AIO1            RESET I/O AREA                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE KEY                                                  
*                                                                               
DK       MVC   MYKEY,KEY           SAVE KEY                                     
         L     R4,AIO              ADDRESS OF THE RECORD                        
         USING TLIFD,R4                                                         
*                                                                               
DK10     MVC   SIFAGY,TLIFAGY      AGENCY REQUIRED                              
         OI    SIFAGYH+6,X'80'     TRANSMIT                                     
         MVC   AIO,AIO2            CHANGE IO TO GET NAME                        
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SIFAGY),SIFAGYNH                      
*                                                                               
DK20     OC    TLIFPCLI,TLIFPCLI   CLIENT                                       
         BZ    DKX                                                              
         MVC   SIFCLI,TLIFPCLI                                                  
         OI    SIFCLIH+6,X'80'     TRANSMIT                                     
         BAS   RE,GETHEX           GET HEX FROM ALPHA CODE IN RECORD            
         BNE   DKX                                                              
         BAS   RE,RDPCLI                                                        
*                                                                               
DKX      MVC   AIO,AIO1            RESTORE IO AREA                              
         MVC   KEY(L'MYKEY),MYKEY                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        GET HEX CODE FROM ALPHA ID IN RECORD                                   
*                                                                               
GETHEX   NTR1                                                                   
         XC    MYFULL,MYFULL                                                    
         L     R4,AIO1             GET INTERFACE ELEMENT                        
         USING TAIFD,R4                                                         
         MVI   ELCODE,TAIFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         ST    R4,MYFULL                                                        
         OC    TAIFAGY,TAIFAGY     ALPHA AGY                                    
         BZ    YES                                                              
         LA    R2,SIFALPHH         SET FOR USERVAL - CAN'T USE W/S              
         MVC   8(L'TAIFAGY,R2),TAIFAGY                                          
         MVI   5(R2),L'TAIFAGY                                                  
         OI    4(R2),X'20'         MARK IT VALIDATED                            
         MVC   AIO,AIO3            CHANGE IO AREA                               
         GOTO1 USERVAL,DMCB,(X'40',SIFALPHH),SIFALPNH                           
         MVC   AIO,AIO1            RESET IO AREA                                
         CLI   TGACCHX,0           IF NO HEX RETURNED                           
         BNE   YES                                                              
         NI    4(R2),X'DF'         MARK IT UN-VALIDATED                         
         B     NO                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         TWAXC SIFALPHH                                                         
         MVC   SIFALPN,SPACES      CLEAR PROTECTED FIELD                        
         OI    SIFALPNH+6,X'80'                                                 
         MVC   SIFVENN,SPACES                                                   
         OI    SIFVENNH+6,X'80'                                                 
*                                                                               
         BAS   RE,GETHEX                                                        
         BNE   DR120                                                            
*                                                                               
         USING TAIFD,R4                                                         
         L     R4,MYFULL           A(INTERFACE ELEMENT)                         
         OC    TAIFVEND,TAIFVEND   VENDOR ACCOUNT                               
         BZ    DR20                                                             
         MVC   SIFVEND,TAIFVEND                                                 
         LA    R2,SIFVENDH                                                      
         BAS   RE,GTVEND           GET VENDOR NAME                              
*                                                                               
DR20     OC    TAIFTV,TAIFTV       TV DEFAULT JOB                               
         BZ    DR35                                                             
         LA    R2,SIFDJOB          DEFAULT JOBS                                 
         OC    SIFDJOB,SPACES                                                   
         MVC   0(L'TAIFTV,R2),TAIFTV                                            
         OC    TAIFRAD,TAIFRAD                                                  
         BZ    DR35                                                             
         CLC   TAIFRAD,TAIFTV      IF THE JOBS ARE THE SAME                     
         BE    DR35                THEN ONLY DISPLAY ONE                        
         LA    R2,12(R2)                                                        
         BAS   RE,SHUFFLE          INSERT COMMA AT END OF STRING                
         MVC   0(L'TAIFRAD,R2),TAIFRAD                                          
*                                                                               
DR35     L     R2,AIO2             R2=A(UNSCAN BLOCK)                           
         ZIC   R0,TAIFNWCS         R0=N'SUB-ELEMENTS IN EL.                     
         LTR   R0,R0                                                            
         BZ    DR120                                                            
         LA    R1,TAIFWCS          R1=A(FIELD IN EL.)                           
*                                                                               
DR40     LA    RF,WCTAB            LOOK UP EQUATE IN TABLE                      
         MVC   0(20,R2),SPACES     INIT THIS ENTRY                              
*                                                                               
DR50     CLI   0(RF),X'FF'         MUST FIND IN TABLE                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),0             IF THIS IS CLIENT                            
         BNE   DR56                                                             
         LA    R1,4(R1)            DO NOT DISPLAY IT - IGNORE                   
         B     DR85                                                             
*                                                                               
DR56     CLC   0(1,R1),0(RF)       IT'S IN 1ST BYTE OF SUB-ELEMENT              
         BE    DR57                                                             
         LA    RF,L'WCTAB(RF)                                                   
         B     DR50                                                             
*                                                                               
DR57     CLI   0(RF),11            FOUND MATCH - IS IT CATEGORY                 
         BNE   DR58                                                             
         LR    R3,R1               SAVE R1                                      
         GOTO1 CATVAL,DMCB,(X'80',1(R3))                                        
         LR    R1,R3               RESTORE R1                                   
         BE    DR57B                                                            
         LA    R1,2(R1)            IF INVALID - IGNORE                          
         B     DR85                                                             
DR57B    MVI   0(R2),C'C'                                                       
         MVC   10(3,R2),TGCACDE    SAVE CATEGORY CODE IN 2ND OPERAND            
         LA    R1,2(R1)            BUMP TO NEXT FIELD IN EL.                    
         B     DR80                                                             
*                                                                               
DR58     CLI   0(RF),12            FOUND MATCH - IS IT USE                      
         BNE   DR59                                                             
         LR    R3,R1               SAVE R1                                      
         GOTO1 USEVAL,DMCB,(X'C0',1(R3))                                        
         LR    R1,R3               RESTORE R1                                   
         BE    DR58B                                                            
         LA    R1,2(R1)            IF INVALID - IGNORE                          
         B     DR85                                                             
DR58B    MVI   0(R2),C'U'                                                       
         MVC   10(3,R2),TGUSCDE    SAVE CATEGORY CODE IN 2ND OPERAND            
         LA    R1,2(R1)            BUMP TO NEXT FIELD IN EL.                    
         B     DR80                                                             
*                                                                               
DR59     MVC   0(3,R2),1(RF)       FOUND MATCH - DISPLAY LITERAL                
         TM    0(RF),X'80'         IS THIS FOR W/C                              
         BO    DR60                                                             
*                                                                               
         LA    R1,1(R1)            BUMP TO NEXT FIELD IN EL.                    
         B     DR80                                                             
*                                                                               
DR60     MVC   10(2,R2),1(R1)      DISPLAY WORK-CODE                            
         LA    R1,3(R1)            BUMP TO NEXT FIELD IN EL.                    
*                                                                               
DR80     LA    R2,20(R2)           BUMP TO NEXT ENTRY IN BLOCK                  
DR85     BCT   R0,DR40             PROCESS NEXT SUB-ELEMENT                     
*                                                                               
         MVC   DMCB,AIO2           SET A(BLOCK)                                 
         ZIC   R0,TAIFNWCS         SET N'ENTRIES TO PRINT                       
         STC   R0,DMCB                                                          
         LA    R2,SIFWCODH         R2=A(FIRST DATA FIELD)                       
         SPACE 1                                                                
DR90     CLI   DMCB,0              TEST ANYTHING (LEFT) TO PRINT                
         BE    DR120                                                            
         GOTO1 UNSCAN,DMCB,,(R2),0,0                                            
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         LA    RF,SIFWC6H          INSURE NOT PAST LAST FIELD                   
         CR    R2,RF                                                            
         BNH   DR90                                                             
*                                                                               
DR120    DS    0H                                                               
         LA    R2,SIFSTAT          DISPLAY STATUS BYTE                          
         OC    MYFULL,MYFULL       IS THERE A TAIF ELEMENT?                     
         BZ    DR125                                                            
         L     R4,MYFULL                                                        
         OC    TAIFBDAY,TAIFBDAY   NUM OF DAYS BEFORE EOM TO SET AS NXT         
         BZ    DR125               MONTH IN ACTIVITY DATE OF POSTINGS           
         MVC   0(6,R2),=C'BDAYS='  (BUSINESS DAYS)                              
         EDIT  TAIFBDAY,(2,6(R2)),ALIGN=LEFT,ZERO=NOBLANK                       
         LA    R2,6(R2)            R2 --> RHS                                   
         AR    R2,R0               ADD LENGTH OF RHS                            
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)            R2 --> NEXT STATUS                           
*                                                                               
DR125    LA    RF,STATTAB          1ST STATUS BYTE                              
*                                                                               
DR130    CLI   0(RF),X'FF'                                                      
         BE    DR160                                                            
         MVC   BYTE,0(RF)                                                       
         NC    BYTE,TAIFSTAT       IS BIT ON IN 1ST STATUS BYTE                 
         BNZ   DR150                  IN ELEMENT                                
*                                                                               
DR140    LA    RF,L'STATTAB(RF)                                                 
         B     DR130                                                            
*                                                                               
DR150    MVC   0(10,R2),1(RF)      YES - DISPLAY LITERAL                        
         LA    R2,10-1(R2)                                                      
         BAS   RE,SHUFFLE          SHUFFLE BACK TO END AND INSERT COMMA         
         B     DR140                                                            
*                                                                               
DR160    BAS   RE,TRAIL            CLEAR TRAILING COMMA IF NECESSARY            
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,SIFT10VH,TAFNTT10                           
*                                                                               
         LA    R2,SIFOFF1H                                                      
         L     R4,AIO                                                           
         USING TAIOD,R4                                                         
         MVI   ELCODE,TAIOELQ      GET INTERFACE OFFICE JOBS                    
         BAS   RE,GETEL                                                         
*                                                                               
DR170    BNE   DR200                                                            
         MVC   8(L'TAIOOFF,R2),TAIOOFF  OFFICE CODE                             
         ZIC   R1,0(R2)            BUMP TO JOB FIELD                            
         AR    R2,R1                                                            
         MVC   8(L'TAIOJOB,R2),TAIOJOB  DEFAULT JOB                             
         ZIC   R1,0(R2)            BUMP TO > FIELD                              
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP TO NEXT OFF FIELD                       
         AR    R2,R1                                                            
         BAS   RE,NEXTEL                                                        
         B     DR170                                                            
*                                                                               
DR200    GOTO1 ACTVOUT,DMCB,SIFLCHGH                                            
*                                                                               
DRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         XC    PTRBLK,PTRBLK       CLEAR POINTER BLOCK                          
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         MVI   ELCODE,TAIFELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAIOELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   LENSET,C'N'         DEFAULT ELEMENT LEN NOT SET                  
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAIFD,R4                                                         
*                                                                               
         OC    SIFALPH,SPACES                                                   
         LA    R2,SIFALPHH                                                      
         TM    4(R2),X'20'                                                      
         BO    BLD5                                                             
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   AIO,AIO3                                                         
         GOTO1 USERVAL,DMCB,(X'40',SIFALPHH),SIFALPNH                           
         MVC   AIO,AIO1                                                         
         CLI   TGACCHX,0           IF NO HEX RETURNED                           
         BE    USERERR             GIVE ERROR                                   
         CLI   SIFCLIH+5,0                                                      
         BE    BLD5                                                             
         BAS   RE,RDPCLI           MAKE SURE PRODUCTION CLIENT EXISTS           
*                                       FOR NEW ALPHA CODE                      
BLD5     MVC   TAIFAGY,SIFALPH                                                  
         OI    4(R2),X'20'                                                      
         LA    R2,SIFVENDH         VALIDATE VENDOR ACCOUNT                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         OC    SIFVEND,SPACES                                                   
         BAS   RE,GTVEND                                                        
         BNE   ERRXIT              ERROR IS SET IN READACC                      
         MVC   TAIFVEND,SIFVEND    SAVE IN ELEMENT                              
*                                                                               
BLD10    LA    R2,SIFWCODH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         BAS   RE,VALWC            VALIDATE WORK CODES                          
         BNE   BLD20               IF INPUT THEN ELEM LEN SET                   
         MVI   LENSET,C'Y'         ELEMENT LENGTH IS ALREADY SET                
*                                                                               
BLD20    LA    R2,SIFSTATH         STATUS                                       
         CLI   5(R2),0                                                          
         BE    BLD75                                                            
         LA    R2,SIFSTATH                                                      
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(X'80',BLOCK)                                  
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R5,BLOCK            R5=A(SCAN BLOCK)                             
         USING SCAND,R5                                                         
*                                                                               
BLD60    MVC   ERRDISP,SCDISP1     SET DISP INTO ERROR                          
*                                                                               
         CLC   =C'BDAYS',SCDATA1   IF END OF MONTH OVERRIDE                     
         BNE   BLD65                                                            
         OC    SCBIN2+3(1),SCBIN2+3  RHS MUST BE NUMERIC                        
         BZ    INVERR                                                           
         CLI   SCBIN2+3,15         NUMBER CAN'T BE GREATER THAN 15              
         BH    INVERR                                                           
         MVC   TAIFBDAY,SCBIN2+3                                                
         B     BLD73                                                            
*                                                                               
BLD65    CLI   SCLEN2,0            INSURE NO INPUT ON RHS                       
         BNE   INVERR                                                           
         LA    RF,STATTAB          LOOP THROUGH STATUS TABLE                    
*                                                                               
BLD70    CLI   0(RF),X'FF'                                                      
         BE    INVERR                                                           
         ZIC   R1,SCLEN1                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),1(RF)    MATCH TO TABLE VALUE                         
         BE    *+12                                                             
         LA    RF,L'STATTAB(RF)                                                 
         B     BLD70                                                            
         OC    TAIFSTAT,0(RF)      TURN ON BIT IN ELEMENT                       
*                                                                               
BLD73    TM    TAIFSTAT,TAIFSPPN   IF PRINTING AT AGENCY                        
         BZ    *+12                                                             
         CLI   SIFCLIH+5,0         THEN ONLY VALID ON AGENCY LEVEL REC          
         BNE   INVERR                                                           
*                                                                               
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         LA    R5,SCANNEXT         BUMP TO IT                                   
         BCT   R0,BLD60            AND CONTINUE                                 
         MVI   ERRDISP,0           CLEAR DISP INTO ERROR                        
*                                                                               
BLD75    CLI   LENSET,C'Y'         IF LENGTH OF RECORD ALREADY SET              
         BE    BLD80               DON'T CHANGE IT                              
         MVI   TAIFLEN,TAIFLNQ     ELEMENT LENGTH                               
*                                                                               
BLD80    LA    R2,SIFDJOBH                                                      
         CLI   5(R2),0                                                          
         BE    BLD85                                                            
         BAS   RE,VALJOB           VALIDATE DEFAULT JOBS                        
*                                                                               
BLD85    MVI   TAIFEL,TAIFELQ      ELEMENT CODE                                 
         GOTO1 ADDELEM                                                          
         XC    ERRDISP,ERRDISP     CLEAR ERROR DISPLACEMENT                     
         DROP  R4,R5                                                            
*                                                                               
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SIFT10VH),TAFNTT10                     
*                                                                               
         LA    R2,SIFOFF1H         DEFAULT OFFICE / JOB                         
         LA    R5,15               CHECK ALL 15 FIELDS                          
*                                                                               
BLD90    CLI   5(R2),0                                                          
         BNE   BLD95                                                            
         ZIC   R1,0(R2)            BUMP PAST JOB FIELD                          
         AR    R2,R1                                                            
         B     BLD100              & CHECK NEXT OFFICE/JOB                      
*                                                                               
BLD95    CLC   SIFDJOB,SPACES      DO NOT ALLOW BOTH DEFAULT JOBS               
         BH    INVERR              & OFFICE/JOBS                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAIOD,R4                                                         
         MVI   TAIOEL,TAIOELQ      ELEMENT CODE                                 
         MVI   TAIOLEN,TAIOLNQ             LENGTH                               
         MVC   WORK(L'TAIOOFF),8(R2)                                            
         OC    WORK(L'TAIOOFF),SPACES                                           
         BAS   RE,VOFF             VALIDATE OFFICE CODE                         
         MVC   TAIOOFF,8(R2)                                                    
         OC    TAIOOFF,SPACES                                                   
*                                                                               
         ZIC   R1,0(R2)            BUMP TO JOB FIELD                            
         AR    R2,R1                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   WORK(12),8(R2)                                                   
         OC    WORK,SPACES                                                      
         BAS   RE,VJOB                                                          
         MVC   TAIOJOB,8(R2)                                                    
         OC    TAIOJOB,SPACES                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
BLD100   ZIC   R1,0(R2)            BUMP TO > FIELD                              
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP TO NEXT OFFICE FIELD                    
         AR    R2,R1                                                            
         BCT   R5,BLD90                                                         
         DROP  R4                                                               
*                                                                               
         GOTO1 ACTVIN,DMCB,SIFLCHGH                                             
*                                                                               
BLDX     L     R4,AIO                                                           
         MVC   KEY,0(R4)                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO VALIDATE WORK-CODE RULES                              
*                                                                               
         SPACE 1                                                                
         USING TAIFD,R4            R4=A(ELEMENT)                                
VALWC    NTR1                                                                   
         LA    R2,SIFWCODH         VALIDATE WORK-CODE DEFINITIONS               
         LA    R3,TAIFWCS          R6=R3=A(FIELD IN ELEMENT)                    
         LR    R6,R3                                                            
         XC    FULL,FULL           FULL=N'SUB-ELEMENTS                          
*                                                                               
VALWC2   CLI   5(R2),0             SKIP IF NOTHING IN FIELD                     
         BE    VALWC14                                                          
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(X'80',BLOCK)                                  
         ZIC   R0,4(R1)            BUMP TOTAL N'SUB-ELEMENTS                    
         A     R0,FULL                                                          
         ST    R0,FULL                                                          
         IC    R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
*                                                                               
VALWC4   LA    R5,WCTAB            LOOK UP LHS IN TABLE                         
         MVC   ERRDISP,SCDISP1     SET DISP INTO ERROR                          
         CLI   SCLEN1,0                                                         
         BE    INVERR                                                           
*                                                                               
VALWC6   CLI   0(R5),X'FF'                                                      
         BE    INVERR              NOT FOUND                                    
***      EX    RE,*+8                                                           
***      B     *+10                                                             
***      CLC   SCDATA1(0),=C'CLIENT'       IF CLIENT - INVALID                  
***      BE    INVERR                                                           
         CLC   1(3,R5),SCDATA1     COMPARE AGAINST FULL LITERAL                 
         BE    VALWC8                                                           
         LA    R5,L'WCTAB(R5)                                                   
         B     VALWC6                                                           
*                                                                               
VALWC8   DS    0H                                                               
         TM    0(R5),X'80'         IF THIS IS W/C                               
         BNO   VALWC10                                                          
*                                                                               
VALWC9   CLI   SCLEN2,2            THEN MUST HAVE WORK-CODE IN RHS              
         BNE   INVERR                                                           
         MVC   AIO,AIO3                                                         
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'           RECORD TYPE                                  
         MVC   KEY+1(1),TGACCHX    COMPANY                                      
         MVC   KEY+2(2),=C'SJ'     U/L                                          
         MVC   KEY+4(2),SCDATA2    WORK CODE                                    
         GOTO1 READACC,DMCB,0      READ ACC FILE                                
         MVC   AIO,AIO1                                                         
         BNE   ERRXIT              ERROR IS SET IN READACC                      
*                                                                               
VALWC10  MVC   0(1,R6),0(R5)       SAVE NUMERIC EQUATE IN ELEMENT               
         CLI   0(R5),11            IF THIS IS CATEGORY                          
         BNE   VALWC10D                                                         
         MVC   ERRDISP,SCDISP2     SET DISP INTO ERROR - RHS                    
         GOTO1 CATVAL,DMCB,SCDATA2                                              
         BNE   ERRXIT                                                           
         MVC   1(1,R6),TGCAEQU     SAVE EQUATE IN ELEMENT                       
         LA    R6,1(R6)            & FURTHER BUMP ELEMENT POINTER               
         B     VALWC12                                                          
*                                                                               
VALWC10D CLI   0(R5),12            IF THIS IS USE                               
         BNE   VALWC11                                                          
         MVC   ERRDISP,SCDISP2     SET DISP INTO ERROR - RHS                    
         GOTO1 USEVAL,DMCB,(X'40',SCDATA2)                                      
         BNE   ERRXIT                                                           
         MVC   1(1,R6),TGUSEQU     SAVE EQUATE IN ELEMENT                       
         LA    R6,1(R6)            & FURTHER BUMP ELEMENT POINTER               
         B     VALWC12                                                          
*                                                                               
VALWC11  TM    0(R5),X'80'         IF THIS IS W/C                               
         BNO   VALWC12                                                          
         MVC   ERRDISP,SCDISP2     SET DISP INTO ERROR - RHS                    
         MVC   1(2,R6),SCDATA2     SAVE WORK-CODE IN ELEMENT                    
         LA    R6,2(R6)            & FURTHER BUMP ELEMENT POINTER               
*                                                                               
VALWC12  LA    R6,1(R6)            BUMP ELEMENT POINTER                         
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         LA    R3,SCANNEXT                                                      
         BCT   R0,VALWC4           AND CONTINUE                                 
*                                                                               
VALWC14  LR    R3,R6               SET R3=A(NEXT FIELD IN ELEMENT)              
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         AR    R2,R0                                                            
         LA    R0,SIFWC6H                                                       
         CR    R2,R0                                                            
         BNH   VALWC2              CONTINUE IF NOT AT END                       
*                                                                               
VALWX    MVC   TAIFNWCS,FULL+3     SAVE N'SUB-ELEMENTS                          
         CLI   TAIFNWCS,0                                                       
         BE    NO                                                               
         SR    R6,R4               A(NEXT SLOT IN EL.) - A(ELEMENT)             
         STC   R6,TAIFLEN          = L'ELEMENT                                  
*                                                                               
         TM    SVAGSTA6,TAAYST10   AGENCY IS TYPE 10?                           
         BZ    YES                                                              
*                                                                               
         MVI   ERRDISP,0           CLEAR DISP INTO ERROR                        
         LA    R2,SIFWCODH                                                      
*                                                                               
         ZIC   R0,TAIFNWCS         N'SUB-ELEMENTS                               
         LA    R3,TAIFWCS                                                       
VALWX02  LA    R1,WCSET1           CHECK IF VALID SET 1                         
VALWX04  CLI   0(R1),X'FF'                                                      
         BE    VALWX08                                                          
         CLC   0(1,R3),0(R1)       SAME WORK CODE?                              
         BNE   VALWX06                                                          
         CLI   2(R1),1             VALID FOR THIS SET?                          
         BNE   VALWX20             NO - TEST SET 2                              
         MVI   2(R1),C'X'          MARK IT SET                                  
         B     VALWX08             YES                                          
*                                                                               
VALWX06  AHI   R1,L'WCSET1         CHECK NEXT ENTRY IN SET 1                    
         B     VALWX04                                                          
*                                                                               
VALWX08  ZIC   RF,1(R1)            BUMP TO NEXT WORK CODE IN ELEMENT            
         AR    R3,RF                                                            
         BCT   R0,VALWX02                                                       
*                                                                               
         LA    R1,WCSET1           TEST ALL REQUIRED CODES ARE SET              
VALWX10  CLI   0(R1),X'FF'                                                      
         BE    YES                 YES - EXIT                                   
         CLI   2(R1),1             VALID WORK CODE NOT SET?                     
         BE    INVERR                                                           
         AHI   R1,L'WCSET1                                                      
         B     VALWX10                                                          
*                                                                               
VALWX20  ZIC   R0,TAIFNWCS         N'SUB-ELEMENTS                               
         LA    R3,TAIFWCS                                                       
VALWX22  LA    R1,WCSET2           CHECK IF VALID SET 2                         
VALWX24  CLI   0(R1),X'FF'                                                      
         BE    VALWX28                                                          
         CLC   0(1,R3),0(R1)       SAME WORK CODE?                              
         BNE   VALWX26                                                          
         CLI   2(R1),1             VALID FOR THIS SET?                          
         BNE   INVERR              NO - ERROR                                   
         MVI   2(R1),C'X'          MARK IT SET                                  
         B     VALWX28             YES                                          
*                                                                               
VALWX26  AHI   R1,L'WCSET1         CHECK NEXT ENTRY IN SET 2                    
         B     VALWX24                                                          
*                                                                               
VALWX28  ZIC   RF,1(R1)            BUMP TO NEXT WORK CODE IN ELEMENT            
         AR    R3,RF                                                            
         BCT   R0,VALWX22                                                       
*                                                                               
         LA    R1,WCSET2           TEST ALL REQUIRED CODES ARE SET              
VALWX30  CLI   0(R1),X'FF'                                                      
         BE    YES                 YES - EXIT                                   
         CLI   2(R1),1             VALID WORK CODE NOT SET?                     
         BE    INVERR                                                           
         AHI   R1,L'WCSET2                                                      
         B     VALWX30                                                          
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
* VALID TYPE 10 WORK CODE SETS                                                  
*                                                                               
* LAYOUT: WORK CODE EQUATE, LENGTH IN TAIFNWCS, 1=VALID 0=INVALID               
*                                                                               
WCSET1   DS    0XL3                                                             
         DC    AL1(10),AL1(4),AL1(1)  RES                                       
         DC    AL1(11),AL1(5),AL1(0)  C=ZZZ                                     
         DC    AL1(12),AL1(5),AL1(1)  U                                         
         DC    AL1(129),AL1(3),AL1(1) ALL                                       
         DC    AL1(131),AL1(3),AL1(1) P&H                                       
         DC    AL1(133),AL1(3),AL1(1) T&H                                       
         DC    AL1(135),AL1(3),AL1(0) HND                                       
         DC    AL1(136),AL1(3),AL1(0) TAX                                       
         DC    X'FF'                                                            
*                                                                               
WCSET2   DS    0XL3                                                             
         DC    AL1(10),AL1(4),AL1(1)  RES                                       
         DC    AL1(11),AL1(5),AL1(1)  C=ZZZ                                     
         DC    AL1(12),AL1(5),AL1(1)  U                                         
         DC    AL1(129),AL1(3),AL1(1) ALL                                       
         DC    AL1(131),AL1(3),AL1(1) P&H                                       
         DC    AL1(133),AL1(3),AL1(0) T&H                                       
         DC    AL1(135),AL1(3),AL1(1) HND                                       
         DC    AL1(136),AL1(3),AL1(1) TAX                                       
         DC    X'FF'                                                            
*                                                                               
*              ROUTINE TO VALIDATE DEFAULT JOB(S)                               
*                                                                               
         USING TAIFD,R4            R4=A(ELEMENT)                                
VALJOB   NTR1                                                                   
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(X'80',BLOCK),C',=,^' SET SO NO RHS            
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         CLI   4(R1),2                                                          
         BH    INVERR                                                           
*                                                                               
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         OC    SCDATA1,SPACES                                                   
         LA    R5,TAIFTV           R5=A(FIELD IN ELEMENT)                       
*                                                                               
VALJO2   CLI   SCLEN1,8            INSURE AT LEAST CLIPRDMJ                     
         BL    INVERR                                                           
         MVC   ERRDISP,SCDISP1     SET DISP INTO ERROR                          
         MVC   WORK(12),SCDATA1                                                 
         BAS   RE,VJOB             VALIDATE ACTUAL JOB                          
*                                                                               
         MVC   0(12,R5),SCDATA1    MOVE JOB TO ELEMENT                          
*                                                                               
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         LA    R5,12(R5)           BUMP TO NEXT                                 
         LA    R3,SCANNEXT                                                      
         BCT   R0,VALJO2           AND CONTINUE                                 
*                                                                               
VALJX    B     XIT                                                              
         DROP  R4,R3                                                            
         EJECT                                                                  
*                                                                               
*        VALIDATE DEFAULT JOB                                                   
*                                                                               
VJOB     NTR1                                                                   
         MVC   AIO,AIO3                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),TGACCHX      COMPANY                                      
         MVC   KEY+1(2),=C'SJ'     U/L                                          
         MVC   KEY+3(12),WORK                                                   
         GOTO1 READACC,DMCB,0      VALIDATE JOB                                 
         MVC   AIO,AIO1                                                         
         BNE   ERRXIT              ERROR IS SET IN READACC                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE DEFAULT OFFICE                                                
*                                  WORK = 2 BYTE OFFICE CODE                    
*                                  R2   = FIELD HEADER                          
*                                                                               
VOFF     NTR1                                                                   
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING A$OGRRECD,R3                                                     
         MVI   A$OGRKTYP,A$OGRKTYPQ                                             
         MVI   A$OGRKSUB,A$OGRKOFFQ OFFICE RECORD                               
         MVC   A$OGRKCPY,TGACCHX    COMPANY                                     
         MVI   A$OGRKUNT,C'S'       UNIT                                        
         MVI   A$OGRKLDG,C'J'       LEDGER                                      
         MVC   A$OGRKOFC,WORK       OFFICE CODE                                 
         GOTO1 READACC,DMCB,0       VALIDATE OFFICE                             
         MVC   AIO,AIO1                                                         
         BNE   ERRXIT                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*        GET VENDOR ACCOUNT REC                                                 
*                                                                               
         USING TAIFD,R4                                                         
GTVEND   NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),TGACCHX                                                   
         MVC   KEY+1(L'TAIFVEND),8(R2)                                          
         MVC   AIO,AIO3            SWITCH IO AREAS TO READ ACC REC              
         GOTO1 READACC,DMCB,SIFVENNH                                            
         MVC   AIO,AIO1            RESTORE IO AREA                              
         B     XIT                 RETURN CC FROM READACC                       
         DROP  R4                                                               
         EJECT                                                                  
*&&UK                                                                           
*                                                                               
*              ROUTINE DETERMINES IF THERE'S ENOUGH ROOM ON A LINE              
*                                  R3=N'BYTES REQUIRED                          
*                                                                               
ENOUGH   DS    0H                  HALF=N'BYTES USED SO FAR                     
         AH    R3,HALF             ADD ONE                                      
         STH   R3,HALF                                                          
         LA    R3,L'SIFWCOD        R3=L'LINE                                    
         CH    R3,HALF                                                          
         BL    NEXTLINE            NOT ENOUGH ROOM - SKIP TO NEXT LINE          
         BER   RE                  RETURN CC EQ IF ENOUGH W/O COMMA             
         LH    R3,HALF                                                          
         AH    R3,=H'1'            ADD ONE TO N'BYTES USED (FOR COMMA)          
         STH   R3,HALF                                                          
         CH    R3,=H'1'                                                         
         BR    RE                  RETURN CC HIGH IF ENOUGH WITH COMMA          
*                                                                               
*              ROUTINE BUMPS TO NEXT WORK-CODE DEFINITION LINE                  
*                                                                               
NEXTLINE DS    0H                  R2=A(CURRENT LOCATION+1)                     
         LR    R3,RE                                                            
         BAS   RE,TRAIL            ERASE TRAILING COMMA                         
         LR    RE,R3                                                            
         LA    R3,SIFWC2                                                        
         CR    R3,R2               FIRST TRY SECOND LINE                        
         BH    NEXTLX                                                           
         LA    R3,SIFWC3           ELSE TRY THIRD                               
         CR    R3,R2                                                            
         BLR   RE                  NO MORE ROOM - RETURN CC LOW                 
*                                                                               
NEXTLX   LR    R2,R3               SET R2=A(NEXT LINE)                          
         XC    HALF,HALF           CLEAR DISPLACEMENT INTO LINE                 
         CH    R2,HALF             RETURN CC HIGH                               
         BR    RE                                                               
*&&                                                                             
         EJECT                                                                  
*                                                                               
         USING SCAND,R3                                                         
ADDISP   NTR1                                                                   
         ZIC   RF,SCLEN1           L'LHS                                        
         ZIC   RE,SCLEN2           + L'RHS                                      
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AH    RE,=H'1'            + '=' SIGN IF THERE IS A RIGHT HALF          
         LA    RF,1(RF,RE)         + DELIMITER                                  
         AH    RF,HALF             + L'SO FAR                                   
         STH   RF,HALF             = CURRENT DISPLACEMENT INTO FIELD            
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 3                                                                
*                                                                               
* ROUTINE TO FIND END OF STRING AND INSERT A COMMA                              
*                                                                               
SHUFFLE  DS    0H                  R2=A(FARTHEST POSS. END OF STRING)           
         CLI   0(R2),C' '          SHUFFLE BACK TO END                          
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','          INSERT COMMA                                 
         LA    R2,2(R2)            RETURN R2=A(NEXT AVAILABLE SLOT)             
         BR    RE                                                               
*                                                                               
*              ROUTINE TO CLEAR TRAILING COMMA                                  
*                                                                               
TRAIL    DS    0H                  R2=A(1 PAST END OF STRING)                   
         BCTR  R2,0                                                             
         CLI   0(R2),C','          IF IT'S AROUND                               
         BNER  RE                                                               
         MVI   0(R2),C' '          CLEAR TRAILING COMMA                         
         BR    RE                                                               
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC               SET CC                                       
*                                                                               
NO       LTR   RC,RC                                                            
         B     XIT                                                              
*                                                                               
USERERR  MVI   ERROR,ERNOID                                                     
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
NTFOUND  MVI   ERROR,NOTFOUND                                                   
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRLEVEL MVI   ERROR,ERAGYLVL      AGENCY LEVEL RECORD MUST EXIST               
         LA    R2,SIFAGYH                                                       
         B     ERRXIT                                                           
*                                                                               
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
WCTAB    DS    0CL4                TABLE TO COVER WORK-CODE KEYWORDS            
*******  DC    AL1(00),CL3'CLI'                                                 
         DC    AL1(01),CL3'MUS'                                                 
         DC    AL1(02),CL3'SIN'                                                 
         DC    AL1(03),CL3'EXT'                                                 
         DC    AL1(04),CL3'OTH'                                                 
         DC    AL1(05),CL3'TV'                                                  
         DC    AL1(06),CL3'RAD'                                                 
         DC    AL1(07),CL3'ON'                                                  
         DC    AL1(08),CL3'OFF'                                                 
         DC    AL1(09),CL3'SES'                                                 
         DC    AL1(10),CL3'RES'                                                 
         DC    AL1(11),CL3'C'                                                   
         DC    AL1(12),CL3'U'                                                   
         DC    AL1(13),CL3'CLB'                                                 
         SPACE 1                                                                
         DC    AL1(129),CL3'ALL'       WORK-CODES START HERE                    
         DC    AL1(130),CL3'FEE'                  (X'80' BIT ON)                
         DC    AL1(131),CL3'P&&H'                                               
         DC    AL1(132),CL3'H&&W'                                               
         DC    AL1(133),CL3'T&&H'                                               
         DC    AL1(134),CL3'CSF'                                                
         DC    AL1(135),CL3'HND'                                                
         DC    AL1(136),CL3'TAX'                                                
         DC    AL1(137),CL3'COM'                                                
*                                                                               
         DC    AL1(138),CL3'GST'                                                
         DC    X'FF'                                                            
         EJECT                                                                  
STATTAB  DS    0CL11               TABLE TO COVER STATUS BYTE                   
         DC    X'80',CL10'UPDPAYABLE'    UPDATE SV POSTING                      
         DC    X'40',CL10'OFFSORT   '    SORT BY OFFICE                         
         DC    X'20',CL10'NOCLINOINT'    IF CLIENT NOT FND, DON'T INT           
         DC    X'10',CL10'NODEFNOINT'    IF DEFAULT JOB NOT FOUND               
*                                           DON'T INTERFACE                     
         DC    X'08',CL10'POSTZERO  '    POST ZERO AMOUNTS                      
         DC    X'04',CL10'NARRATIVE '    WANT NARRATIVE ON SJ POSTINGS          
         DC    X'02',CL10'PRTPN     '    PRINT TPN AT AGENCY                    
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR25D                                                       
         EJECT                                                                  
         ORG   SIFWORK                                                          
MYFULL   DS    F                   TEMP STORAGE                                 
SVAGY    DS    CL6                 SAVED AGENCY                                 
SVPCLI   DS    CL6                 SAVED PRODUCTION CLIENT                      
MYKEY    DS    CL38                SAVED KEY                                    
LENSET   DS    CL1                 SWITCH IS LENGTH SET                         
SVAGSTA6 DS    XL1                 AGENCY STATUS 6                              
MYBLOCK  DS    CL25                BUILD SUBSIDIARY ELEMENT HERE                
PTRBLK   DS    CL((1*L'TLDRREC)+1)                                              
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE TASYSDSECT                                                     
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTGENFILE                                                                     
* ACGENFILE  (PREFIX=A$)                                                        
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE CTGENFILE                                                      
*PREFIX=A$                                                                      
       ++INCLUDE ACGENFILE                                                      
*PREFIX=                                                                        
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047TAGEN25   02/05/13'                                      
         END                                                                    
