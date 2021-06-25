*          DATA SET GEKWX01A   AT LEVEL 023 AS OF 05/01/02                      
*PHASE TF2001A,+0                                                               
         TITLE '$KWX MK3 - BOOK AND FORMAT ACTIONS'                             
***********************************************************************         
*  HISTORY OF CHANGES                                                 *         
***********************************************************************         
*  24FEB93 (BU ) - CHANGE # DEFAULT DAYS FROM 3000 TO 2000 BECAUSE    *         
*                  DATE GENERATED IS IN NEXT CENTURY, YEAR TEST FAILS,*         
*                  AND ITEM IS PURGED FROM SYSTEM IMMEDIATELY.        *         
*                                                                     *         
*  21JUN95 (BU ) - 'HELP' FIELD OVERFLOW:  SHORTEN LENGTH OF DISPLAY  *         
*                  REP NAME.                                          *         
*                                                                     *         
*  14JUL95 (SKU) - 'HARD-CODE' REP3 TEMPORARILY                       *         
*                                                                     *         
*  21JUL95 (BU ) - CHANGE # DEFAULT DAYS FROM 2000 TO 1000 BECAUSE    *         
*                  DATE GENERATED IS IN NEXT CENTURY, YEAR TEST FAILS,*         
*                  AND ITEM IS PURGED FROM SYSTEM IMMEDIATELY.        *         
*                  ***  END TOMBSTONE  ***                            *         
*                                                                     *         
*  21MAR96 (SKU) - ADD ENTRY FOR TCH8                                 *         
*                                                                     *         
*  11DEC98 (RHV) - ALLOW RETENTION=KEEP                               *         
***********************************************************************         
KWX01    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**KWX01*,R9                                                    
         L     RC,0(R1)                                                         
         USING KWX01+4096,R9       R9 = 2ND BASE                                
         USING TWAD,RA             RA = TWA                                     
         USING GWS,RC              RC = GWS                                     
*                                                                               
T001     ST    RB,ABASE2                                                        
         ST    R9,A2NDBAS2                                                      
         ST    RD,AREGSAV2                                                      
         B     T002                                                             
         EJECT                                                                  
*              FURTHER CHECKS ON PRE-PROCESSED PARAMETERS - FORM ACTION         
*                                                                               
T002     CLI   ACTION,FOR          IF REF GIVEN, FORM NAME REQUIRED             
         BNE   T010                                                             
         CLI   FXFORMID,0                                                       
         BNE   T004                                                             
         CLI   FXREF,0                                                          
         BE    T004                                                             
         MVI   FERN,MISSING                                                     
         MVI   FNDX,1                                                           
         MVI   SUBFNDX,2                                                        
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T004     CLC   NUM,=H'18'          NUMBER OF CHUNKS LEQ 18                      
         BNH   T006                                                             
         MVI   FERN,NOVERMAX                                                    
         MVC   FNDX,FXNUM                                                       
         B     ERROR                                                            
*                                                                               
T006     CLI   FXREF,0             REF RANGE NOT GTR THAN NUMBER                
         BNE   *+12                                                             
         MVI   REFLO+1,1                                                        
         B     T230                                                             
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R0,3,REFLO                                                       
         ICM   R1,3,REFHI                                                       
         SR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         CH    R1,=H'1'                                                         
         BE    T230                                                             
         CLI   FXNUM,0                                                          
         BE    T008                                                             
         CH    R1,NUM                                                           
         BE    T230                                                             
         MVI   FERN,INCPARMS                                                    
         MVC   FNDX,FXNUM                                                       
         B     ERROR                                                            
*                                                                               
T008     STCM  R1,3,NUM            REF RANGE, IF GIVEN, SETS NUM LIMIT          
         B     T230                                                             
         EJECT                                                                  
*              FURTHER PARAMETER CHECKS ON PRE-PROCESSED PARAMETERS             
*              BOOK ACTION                                                      
*                                                                               
T010     CLI   FXID,0              NO ID PARAMETER INVALID UNLESS ADD           
         BNE   T015                                                             
         CLI   FXADD,0                                                          
         BNE   T020                                                             
         MVI   FERN,MISSING                                                     
         MVI   FNDX,2                                                           
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T015     MVC   WORK(2),TWAUSRID     IF USER IS NOT THE OWNER, HE CANT           
         MVI   WORK+2,C'K'         DO THE FOLLOWING                             
         MVC   WORK+3(3),LASTINIT                                               
         CLI   SAVMODE,FORMAT                                                   
         BE    *+10                                                             
         MVC   WORK+2(4),LASTINIT                                               
         CLC   ID(6),WORK                                                       
         BE    T020                                                             
         MVI   FERN,INVNOWNR                                                    
         MVI   FNDX,0                                                           
         OC    FNDX,FXADD          ADD IT                                       
         BNZ   ERROR                                                            
         OC    FNDX,FXDEL          DELETE IT                                    
         BNZ   ERROR                                                            
         OC    FNDX,FXCOMM         CHANGE COMMENTS                              
         BNZ   ERROR                                                            
         OC    FNDX,FXNUM          CHANGE RETENTION DAYS                        
         BNZ   ERROR                                                            
         OC    FNDX,FXCLASS                                                     
         BNZ   ERROR               CHANGE CLASSES                               
*                                                                               
T020     CLI   FXADD,0             CANT ADD AND DELETE                          
         BE    T029                                                             
         CLI   FXDEL,0                                                          
         BE    T025                                                             
         MVC   FNDX,FXADD                                                       
         MVI   FERN,INCPARMS                                                    
         MVI   SYNTAX,C'Y'                                                      
         CLC   FNDX,FXDEL                                                       
         BH    ERROR                                                            
         MVC   FNDX,FXDEL                                                       
         B     ERROR                                                            
*                                                                               
T025     CLI   FXNUM,0             RETENTION ON FORMAT BOOKS DEFAULTS           
         BNE   T028                TO KEEP AND ON MESSAGE BOOKS TO 1            
         MVI   FXNUM,C'X'                                                       
         MVC   NUM,=H'1'           EFFECTIVE 1/17/85                            
         CLI   SAVMODE,FORMAT                                                   
         BNE   T028                                                             
****>>>  MVC   NUM,=H'2000'        EFFECTIVE 24FEB93                            
         MVC   NUM,=X'FFFF'        EFFECTIVE 10DEC98                            
*                                                                               
T028     CLI   SAVMODE,MESSAGE     FORM=NAME HAS NO MEANING IN FORM MOD         
         BE    T029                                                             
         CLI   FXFORMID,0                                                       
         BE    T029                                                             
         MVC   FNDX,FXFORMID                                                    
         MVI   FERN,INVALID                                                     
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T029     CLI   FXCLASS,0           CLASS=A+B+C+D                                
         BE    T030                                                             
         MVI   BYTE,0              BUILD HDMSTAT BITS IN BYTE                   
         SR    RE,RE               RE = SUBFNDX                                 
         CLI   SAVMODE,MESSAGE                                                  
         BE    T029E                                                            
         CLC   CLASS(2),=C'0 '     ALTERNATIVE WAYS OF CLEARING CLASS           
         BE    T030                                                             
         CLC   CLASS(2),=C'NO'                                                  
         BE    T030                                                             
         CLC   CLASS(3),=C'NIL'                                                 
         BE    T030                                                             
         CLI   CLASS+1,C' '                                                     
         BNH   *+8                                                              
         LA    RE,1                                                             
         LA    R1,CLASS                                                         
T0291    LA    R2,CLASSTAB                                                      
T0292    CLI   0(R2),0                                                          
         BE    T029E1                                                           
         CLC   0(1,R1),0(R2)                                                    
         BE    *+12                                                             
         LA    R2,2(R2)                                                         
         B     T0292                                                            
         OC    BYTE,1(R2)                                                       
         CLI   1(R1),C' '                                                       
         BNH   T030                                                             
         CLI   1(R1),C'+'                                                       
         BNE   T029E                                                            
         LA    R1,2(R1)                                                         
         LA    RE,1(RE)                                                         
         B     T0291                                                            
T029E    MVI   SYNTAX,C'Y'                                                      
T029E1   MVI   FERN,INVALID                                                     
         MVC   FNDX,FXCLASS                                                     
         STC   RE,SUBFNDX                                                       
         B     ERROR                                                            
*                                                                               
CLASSTAB DC    C'A',X'10'          INITIALS MUST BE A VALID ID                  
         DC    X'00'                                                            
         EJECT                                                                  
*              CHECK WHETHER THE BOOK EXISTS - DELETE IT IF REQUIRED            
*                                                                               
T030     XC    SAVEREST,SAVEREST   CLEAR BOOK-RELATED VALUES                    
         XC    BUFFSAVE,BUFFSAVE                                                
         XC    KWXBOOK,KWXBOOK                                                  
         OI    KWXBOOKH+6,X'80'                                                 
*                                                                               
T035     GOTO1 ,PARAS,(SAVMODE,ID) IF NO ID GIVEN, FIND NEXT AVAILABLE          
         L     RF,AFINDBK                                                       
         CLI   FXID,0                                                           
         BNE   T040                                                             
         BASR  RE,RF                                                            
         BZ    ERROR               NONE AVAILABLE                               
         B     T125                ID NOW CONTAINS NEXT AVAILABLE               
*                                                                               
T040     BASR  RE,RF               IF ID GIVEN, DOES BOOK EXIST                 
         TM    DMCB+8,X'6F'                                                     
         BNZ   ERROR                                                            
         TM    DMCB+8,X'10'                                                     
         BO    T120                NOT FOUND                                    
*                                                                               
T045     CLI   ADD,C'Y'            BOOK EXISTS - ERROR IF ADD                   
         BNE   T050                                                             
         MVI   FERN,BOOKXIST                                                    
         MVC   FNDX,FXADD                                                       
         B     ERROR                                                            
*                                                                               
T050     CLI   DEL,C'Y'            DELETE BOOK                                  
         BNE   T060                                                             
         LA    R0,=C'DEL'                                                       
         ST    R0,DMCB                                                          
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         MVC   KWXHEAD(14),=C'BOOK DELETED -'                                   
         LA    R1,KWXHEAD+15                                                    
         ST    R1,FULL                                                          
         B     NEXTACT                                                          
         EJECT                                                                  
*              PROCESS FOR EXISTING BOOK REQUIRED FOR FURTHER USE               
*                                                                               
T060     L     R4,DMCB+12          HANDLE COMMENTS AND RETENTION                
         XC    0(4,R4),0(R4)                                                    
         LA    R4,28(R4)                                                        
         USING WKRECD,R4                                                        
         LA    R0,=C'RAN'                                                       
         ST    R0,DMCB                                                          
         GOTO1 ADATAMGR,DMCB       RANDOM READ FOR REC 0 (ATTRIBUTES)           
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         CLI   FXCOMM,0            COMMENT CHANGE                               
         BE    T065                                                             
         MVC   WKCOMNT,COMMENT                                                  
         LA    R0,=C'COM'                                                       
         ST    R0,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
T065     CLI   FXNUM,0             RETENTION CHANGE                             
         BE    T068                                                             
         MVC   WKRETN,NUM                                                       
         LA    R0,=C'RET'                                                       
         ST    R0,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
T068     MVC   KWXHELP(8),=C'COMMENT='  DISPLAY COMMENT/CREATE DATE/RET         
         MVC   KWXHELP+8(L'WKCOMNT),WKCOMNT                                     
         OC    WKCOMNT,WKCOMNT                                                  
         BNZ   *+10                                                             
         MVC   KWXHELP+8(4),=C'NONE'                                            
         LA    R3,KWXHELP+7+L'WKCOMNT                                           
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVC   1(15,R3),=C', DATE CREATED='                                     
         GOTO1 ADATCON,PARAS,(1,WKDATEC),(8,16(R3))                             
         LA    R3,16+8(R3)                                                      
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVC   1(6,R3),=C', RET='                                               
         OC    WKRETN,WKRETN       RETENTION=0?                                 
         BZ    T069                YES - KEEP STATUS                            
         EDIT  WKRETN,(4,7(R3)),ALIGN=LEFT                                      
         OI    7(R3),C'0'                                                       
         B     *+14                                                             
T069     DS    0H                                                               
         MVC   7(4,R3),=C'KEEP'                                                 
         LH    R0,=H'4'                                                         
*                                                                               
         AR    R3,R0                                                            
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R3,1(R3)                                                         
         MVC   8(4,R3),=C'DAYS'                                                 
         LA    R1,12(R3)                                                        
         CLC   WKRETN,=H'1'                                                     
         BNE   *+10                                                             
         MVI   17(R3),C' '                                                      
         BCTR  R1,0                                                             
         ST    R1,FULL             SAVE A(NEXT POSN IN HELP LINE)               
         DROP  R4                                                               
*                                                                               
T070     L     R4,DMCB+12          GET HEADER RECORD                            
         GOTO1 AGETCHNK,PARAS,(SAVMODE,0)                                       
         BZ    ERROR               NOT FOUND                                    
         USING HDRD,R4                                                          
         CLC   HDTYPE,SAVMODE      COMPATIBLE MODE OF BOOK                      
         BNE   T079                                                             
         CLC   TWAUSRID,ID                                                      
         BNE   T075                                                             
         CLI   SAVMODE,FORMAT                                                   
         BE    *+18                                                             
         CLC   LASTINIT(4),ID+2                                                 
         BNE   T075                                                             
         B     *+14                                                             
         CLC   LASTINIT(3),ID+3                                                 
         BNE   T075                                                             
         MVI   SAVACCS,WRITACC     OWNER AUTOMATICALLY GETS WRITE ACCSS         
         B     T080                                                             
*                                                                               
T075     LA    RF,HDACCS           CHECK AUTHORIZATION OTHERWISE                
T076     CLI   0(RF),X'FF'                                                      
         BE    T078                                                             
         CLC   TWAUSRID,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,L'HDACCS(RF)                                                  
         B     T076                                                             
         MVC   SAVACCS,2(RF)                                                    
T078     CLI   SAVACCS,0                                                        
         BNE   T080                                                             
T079     MVI   FERN,NOTAUTH                                                     
         MVC   FNDX,FXID                                                        
         XC    KWXHELP,KWXHELP                                                  
         B     ERROR                                                            
*                                                                               
T080     L     R3,DMCB+8           FORMAT MODE - DISPLAY FIRST CHUNK            
         USING UKRECD,R3           OR IF NONE REQUEST INPUT                     
         CLI   SAVMODE,FORMAT                                                   
         BNE   T090                                                             
         MVC   FRMBOOK,UKKEY       SAVE SOME VALUES                             
         MVC   FRMRECHI,HDFRMHI                                                 
         MVC   FRMRPEAT,HDFRMRPT                                                
*                                                                               
T082     CLI   FXCLASS,0           FIRST HANDLE CLASS PARM                      
         BE    T083                                                             
         NI    HDMSTAT,X'E0'                                                    
         OC    HDMSTAT,BYTE                                                     
         GOTO1 APUTCHNK,PARAS,,(R4)                                             
         BZ    ERROR                                                            
T083     TM    HDMSTAT,X'1F'       AND DISPLAY CLASS=A+B+C IN HELP LINE         
         BZ    T084                                                             
         L     R1,FULL             (FULL=SAVED HELP LINE AD)                    
         MVC   0(8,R1),=C', CLASS='                                             
         LA    R1,8(R1)                                                         
         LA    R2,CLASSTAB                                                      
T0831    CLI   0(R2),0                                                          
         BE    T0833                                                            
         MVC   BYTE,1(R2)                                                       
         NC    BYTE,HDMSTAT                                                     
         BZ    T0832                                                            
         MVC   0(1,R1),0(R2)                                                    
         MVI   1(R1),C'+'                                                       
         LA    R1,2(R1)                                                         
T0832    LA    R2,2(R2)                                                         
         B     T0831                                                            
T0833    BCTR  R1,0                                                             
         MVI   0(R1),C' '                                                       
*                                                                               
T084     XC    PARAS(12),PARAS                                                  
         GOTO1 ASETSCRN,PARAS      SET UP FORMAT DEFINITION SCREEN              
         GOTO1 AGETCHNK,PARAS,(SAVMODE,1)                                       
         BNZ   T085                NO CHUNKS TO DISPLAY / INVITE INPUT          
         MVC   KWXHEAD(12),=C'BOOK FOUND -'                                     
         LA    R1,KWXHEAD+13                                                    
         ST    R1,FULL                                                          
         B     ENTERF                                                           
*                                                                               
T085     DS    0H                  DISPLAY FIRST CHUNK                          
         L     RE,AIOB             MOVE REC FROM IOB TO IO                      
         LH    RF,0(RE)                                                         
         LA    R0,IO                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 ADISFORM,PARAS,1                                                 
         XC    PARAS(12),PARAS                                                  
         MVC   PARAS(4),DISPLOF                                                 
         MVC   PARAS+10(2),FRMRECHI                                             
         B     REFDISP                                                          
*                                                                               
T090     MVC   MSGBOOK,UKKEY       MESSAGE MODE - DISPLAY 1ST SCRNFULL          
         MVC   SAVM,UKRECD                                                      
         MVC   MSGRECHI,HDMSGHI                                                 
         MVC   FRECMHI,HDFRECMH                                                 
         MVC   MSGSTAT,HDMSTAT                                                  
         MVC   FRMBOOK,HDFRMBK                                                  
         MVC   FRMRECHI,HDFRMHI                                                 
         MVC   FRMRPEAT,HDFRMRPT                                                
         GOTO1 ADISSCRN,PARAS,1,0                                               
*                                                                               
T095     OC    FRMBOOK,FRMBOOK     IF MESSAGE BOOK CROSS-REFERS TO A            
         BZ    T100                FORMAT BOOK, CHECK THAT IT EXISTS            
         GOTO1 ,PARAS,FRMBOOK                                                   
         MVI   PARAS,FORMAT                                                     
         L     RF,AFINDBK                                                       
         BASR  RE,RF                                                            
         BNZ   T098                                                             
         XC    FRMBOOK,FRMBOOK                                                  
         XC    FRMRECHI,FRMRECHI                                                
         XC    FRMRPEAT,FRMRPEAT                                                
         GOTO1 AGETCHNK,PARAS,(SAVMODE,0)                                       
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         XC    HDFRMBK(12),HDFRMBK IF IT DOESN'T, REMOVE CROSS-REFS             
         GOTO1 APUTCHNK,PARAS,,IO                                               
         B     T100                                                             
T098     GOTO1 ACLOSE              IF IT DOES SAVE D/ADDR ETC.                  
*                                                                               
T100     OC    DISPLOM(4),DISPLOM  NO CHUNKS TO DISPLAY - REQUEST               
         BNZ   T105                FORM ACTION IF FORM NOT GIVEN                
         MVC   KWXHEAD(10),=C'BOOK FOUND'                                       
         LA    R1,KWXHEAD+13                                                    
         ST    R1,FULL                                                          
         CLI   FXFORMID,0                                                       
         BE    WHCHFORM                                                         
         B     T200                OTHERWISE HANDLE FORM=                       
*                                                                               
T105     XC    PARAS(12),PARAS     CHUNKS DISPLAYED - HIT ENTER FOR NXT         
         MVC   PARAS(4),DISPLOM    OR ENTER NEXT ACTION                         
         MVC   PARAS+10(2),MSGRECHI OR HIT ENTER FOR NEW FORM                   
         B     REFDISP                                                          
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ADD A NEW BOOK                                                   
*                                                                               
T120     CLI   ADD,C'Y'            ADD PARAMETER MUST BE PRESENT                
         BE    T125                                                             
         MVI   FERN,BOOKNXST                                                    
         MVC   FNDX,FXID                                                        
         B     ERROR                                                            
*                                                                               
T125     LM    R3,R4,DMCB+8        PREPARE OPEN CALL                            
         USING UKRECD,R3                                                        
         LA    R4,28(R4)                                                        
         USING WKRECD,R4                                                        
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKKEY,ID                                                         
         MVI   UKFLAG,X'02'        NO DUPS                                      
         CLI   FXCOMM,0                                                         
         BE    *+14                                                             
         OI    UKFLAG,X'08'        COMMENTS                                     
         MVC   WKCOMNT,COMMENT                                                  
*                                                                               
         CLI   FXNUM,0                                                          
         BE    T127                                                             
         CLC   NUM,=X'FFFF'        KEEP?                                        
         BNE   *+18                                                             
         NI    UKFLAG,X'FF'-X'10'  KEEP STATUS                                  
         XC    WKRETN,WKRETN       NO RETENTION DAYS                            
         B     T127                                                             
         OI    UKFLAG,X'10'        RETENTION                                    
         MVC   WKRETN,NUM                                                       
*                                                                               
T127     DS    0H                                                               
         LA    R0,=C'OPE'                                                       
         ST    R0,DMCB                                                          
         GOTO1 ADATAMGR,DMCB       OPEN                                         
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
*                                                                               
T130     L     R4,12(R1)           ADD USER HEADER RECORD                       
         USING HDRD,R4                                                          
         LA    RE,HDRLENQ                                                       
         STH   RE,HDRLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    HDRD+2(0),HDRD+2                                                 
         MVC   HDTYPE,SAVMODE                                                   
         MVI   HDACCS,X'FF'                                                     
         CLI   FXCLASS,0           CLASS=                                       
         BE    *+10                                                             
         MVC   HDMSTAT,BYTE                                                     
         LA    R0,=C'ADD'                                                       
         ST    R0,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         MVI   SAVACCS,WRITACC                                                  
         LA    R0,=C'CLO'                                                       
         ST    R0,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         OC    BUFSTAT,SAVMODE                                                  
*                                                                               
T135     CLI   SAVMODE,FORMAT      NEW FORMAT BOOK                              
         BNE   T140                                                             
         MVC   FRMBOOK,UKKEY                                                    
         XC    PARAS(12),PARAS                                                  
         GOTO1 ASETSCRN,PARAS      SET UP FORMAT SCREEN                         
         LA    R1,FRMBOOK+6                                                     
         BAS   RE,BOOKNUM          DISPLAY 'BOOK NN ADDED -'                    
         B     ENTERF                      'ENTER FORMAT DEFINITION'            
*                                                                               
T140     DS    0H                  NEW MESSAGE BOOK                             
         MVC   MSGBOOK,UKKEY       DISPLAY 'BOOK NN ADDED - SPECIFY             
         LA    R1,MSGBOOK+6                 WHICH SCREEN YOU REQUIRE'           
         BAS   RE,BOOKNUM                                                       
         CLI   FXFORMID,0                                                       
         BE    WHCHFORM                                                         
         B     T200                                                             
*                                                                               
BOOKNUM  DS    0H                  CONVERT BOOKNUM X'2310' TO C'123'            
         MVC   KWXHEAD(16),=C'BOOK NNN ADDED -'                                 
         UNPK  KWXHEAD+5(1),1(1,R1)                                             
         OI    KWXHEAD+5,X'F0'                                                  
         ZIC   R0,0(R1)                                                         
         SRDL  R0,4                                                             
         SLL   R0,4                                                             
         SLDL  R0,4                                                             
         STH   R0,HALF                                                          
         OC    HALF,=C'00'                                                      
         MVC   KWXHEAD+6(2),HALF                                                
         LA    R1,KWXHEAD+17       AND PASS BACK NEXT HEAD POS IN FULL          
         ST    R1,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
         EJECT                                                                  
*              HANDLE FORMATS IN THE THREE CASES -                              
*              (A) FORMAT ACTION                                                
*              (B) BOOK ACTION ADDING A BOOK AND SPECIFYING FORMAT              
*              (C) BOOK ACTION GETTING AN EMPTY BOOK AND DITTO                  
*                                                                               
T200     XC    NUM,NUM             IF BOOK CLEAR NUM (RETENTION OR MAX)         
         MVC   REFLO,=H'1'         AND SET FIRST FORM CHUNK NUM                 
*                                                                               
T230     XC    FRMBOOK,FRMBOOK     FORMAT ACTION COMES IN HERE - INTLSE         
         XC    FRMRECHI,FRMRECHI                                                
         XC    FRMRPEAT,FRMRPEAT                                                
         XC    FRECMHI,FRECMHI                                                  
         MVI   FRMSTAT,0                                                        
         CLI   FXFORMID,0          IF NO ID GIVEN DEFAULT FORM REQUIRED         
         BE    *+14                REQUIRED = 1 REPEATING PSEUDO-RECORD         
         OC    FORMID,FORMID                                                    
         BNZ   T235                                                             
         MVI   FRMRPEAT+1,1                                                     
         MVI   FRMRECHI+1,1                                                     
         B     T250                                                             
*                                                                               
T235     GOTO1 ,PARAS,FORMID       DOES FORMAT BOOK EXIST                       
         MVI   PARAS,FORMAT                                                     
         L     RF,AFINDBK                                                       
         BASR  RE,RF                                                            
         TM    DMCB+8,X'6F'                                                     
         BNZ   ERROR                                                            
         TM    DMCB+8,X'10'                                                     
         BZ    T240                                                             
*                                                                               
T237     MVC   FNDX,FXFORMID       NO BOOK OR NOTHING IN IT                     
         CLI   ACTION,FOR                                                       
         BNE   *+12                                                             
         MVI   FERN,NOFORMAT                                                    
         B     ERROR                                                            
         L     R1,FULL                                                          
         MVC   0(26,R1),=C'BUT FORMAT NOT FOUND - FLD'                          
         EDIT  FXFORMID,(1,27(R1))                                              
         B     OKXIT                                                            
*                                                                               
T240     XC    1(3,R1),1(R1)       BOOK EXISTS - GET HDR                        
         GOTO1 AGETCHNK                                                         
         BZ    ERROR                                                            
         LM    R3,R4,DMCB+8                                                     
         USING UKRECD,R3                                                        
         USING HDRD,R4                                                          
         CLI   HDTYPE,FORMAT                                                    
         BNE   T244                                                             
         CLC   UKUSRID,TWAUSRID    CHECK AUTHORIZATION TO READ - IF             
         BE    T245                OWNER=USER/PRINCIPAL USER/TCH1, OK           
*&&UK                                                                           
         CLC   UKUSRID,SAVTKWID                                                 
         BE    T245                                                             
*&&                                                                             
*&&US                                                                           
         CLC   UKUSRID,=H'43'      TCH1 IN US                                   
         BE    T245                                                             
         CLC   UKUSRID,=H'237'     TCH3 IN US                                   
         BE    T245                                                             
         CLC   UKUSRID,=H'262'     TCH4 IN US                                   
         BE    T245                                                             
         CLC   UKUSRID,=H'316'     TCH5 IN US                                   
         BE    T245                                                             
         CLC   UKUSRID,=H'574'     TCH7 IN US                                   
         BE    T245                                                             
         CLC   UKUSRID,=H'1288'    TCH8 IN US                                   
         BE    T245                                                             
*&&                                                                             
         LA    RF,HDACCS                                                        
T242     CLI   0(RF),X'FF'                                                      
         BE    T244                                                             
         CLC   TWAUSRID,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,L'HDACCS(RF)                                                  
         B     T242                                                             
         TM    2(RF),READACC       READ ACCESS IS SUFFICIENT                    
         BO    T245                                                             
T244     MVC   FNDX,FXFORMID                                                    
         CLI   ACTION,FOR                                                       
         BNE   *+12                                                             
         MVI   FERN,NOTAUTH                                                     
         B     ERROR                                                            
         L     R1,FULL                                                          
         MVC   0(40,R1),=C'BUT ACCESS TO FORMAT NOT PERMITTED - FLD'            
         EDIT  FXFORMID,(1,42(R1))                                              
         B     OKXIT                                                            
*                                                                               
T245     TM    HDMSTAT,X'10'       RESTRICTED FORMAT - INITIALS MUST BE         
         BNO   T248                A VALID ID                                   
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,SPACES                                                    
         MVC   CTIKID(L'LASTINIT),LASTINIT                                      
         GOTO1 ADATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,KEY,WORK                 
         CLI   DMCB+8,0                                                         
         BE    T248                                                             
         LA    R1,KWXINITH                                                      
         ST    R1,ACURSOR                                                       
         MVI   FERN,INITINVF                                                    
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
T248     OC    HDFRMHI,HDFRMHI     MUST BE A CHUNK IN THE BOOK                  
         BZ    T237                                                             
         MVC   FRMBOOK,UKKEY       SAVE FORMAT DATA IN TWA                      
         MVC   FRMRECHI,HDFRMHI                                                 
         MVC   FRMRPEAT,HDFRMRPT                                                
         GOTO1 ACLOSE              INCLUDING INDEX ENTRY/DISKADDR               
*                                                                               
T250     DS    0H                  AND IN MESSAGE BOOK HEADER                   
         GOTO1 AGETCHNK,PARAS,(SAVMODE,0)                                       
         BZ    ERROR                                                            
         L     R4,DMCB+12                                                       
         MVC   HDFRMBK,FRMBOOK                                                  
         MVC   HDFRMHI,FRMRECHI                                                 
         MVC   HDFRMRPT,FRMRPEAT                                                
         XC    HDFRECMH,HDFRECMH                                                
         GOTO1 APUTCHNK,(R1),,IO                                                
         BZ    ERROR                                                            
         B     T260                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*              DISPLAY INPUT SCREEN                                             
*                                                                               
T260     XC    PARAS(12),PARAS                                                  
         MVC   PARAS+2(2),REFLO    START FORMAT CHUNK                           
         LH    R1,MSGRECHI                                                      
         LA    R1,1(R1)                                                         
         STH   R1,PARAS+6          START MESSAGE CHUNK NUMBER                   
         MVC   PARAS+10(2),NUM     MAX CHUNKS OR ZERO                           
         GOTO1 ASETSCRN,PARAS                                                   
         NI    FRMSTAT,ALL-PARTSCRN SET INDICATOR IF RESTRICTED LENGTH          
         OC    NUM,NUM                                                          
         BZ    T265                                                             
         OI    FRMSTAT,PARTSCRN                                                 
*                                                                               
T265     CLI   ACTION,FOR          ASK FOR INPUT                                
         BNE   T267                                                             
         MVC   KWXHEAD(18),=C'ACTION COMPLETED -'                               
         LA    R1,KWXHEAD+18                                                    
         B     *+8                                                              
T267     L     R1,FULL                                                          
         MVC   0(17,R1),=C'NOW INPUT MESSAGE'                                   
         XC    KWXACT,KWXACT                                                    
         MVC   KWXACT(3),=C'ADD'                                                
         OI    KWXACTH+6,X'80'                                                  
         LA    R2,KWXDATAH                                                      
         ST    R2,ACURSOR                                                       
         B     OKXIT                                                            
*              EXITS BACK TO ROOT                                               
*                                                                               
*              (A) REFDISP - PARAS SET FOR EDITREF CALL                         
REFDISP  GOTO1 AEDITREF,PARAS,,KWXHEAD                                          
         L     R1,4(R1)                                                         
         MVC   0(11,R1),=C'DISPLAYED -'                                         
         LA    R1,12(R1)                                                        
         ST    R1,FULL                                                          
         CLI   FXFORMID,0                                                       
         BE    *+14                                                             
         CLC   FRMBOOK,FORMID                                                   
         BNE   NEWFORM                                                          
         CLC   PARAS+2(2),PARAS+10 ANY MORE TO DISPLAY                          
         BE    NEXTACT             NO                                           
         B     HITENT              YES                                          
         SPACE 1                                                                
*              (B) PART 2 OF HEADS - FULL=A(NEXT HEADS POS'N)                   
*                                                                               
NEWFORM  L     R1,FULL                                                          
         MVC   0(22,R1),=C'HIT ENTER FOR NEW FORM'                              
         ZIC   R1,FXFORMID                                                      
         BCTR  R1,0                                                             
         LA    R2,22+L'OLD                                                      
         MR    R0,R2                                                            
         LA    R1,SCANBLCK(R1)                                                  
         XC    KWXACT,KWXACT                                                    
         MVC   KWXACT(7),=C'FORMAT='                                            
         MVC   KWXACT+7(L'KWXACT-7),22(R1)                                      
         B     HITENT2                                                          
*                                                                               
NEXTACT  L     R1,FULL                                                          
         MVC   0(17,R1),=C'ENTER NEXT ACTION'                                   
         B     OKXIT                                                            
*                                                                               
WHCHFORM L     R1,FULL                                                          
         MVC   0(27,R1),=C'WHICH FORM DO YOU REQUIRE ?'                         
         XC    KWXACT,KWXACT                                                    
         MVC   KWXACT(7),=C'FORMAT='                                            
         MVI   KWXACT+7,C'?'                                                    
         OI    KWXACTH+6,X'80'                                                  
         B     OKXIT                                                            
*                                                                               
ENTERF   L     R1,FULL                                                          
         MVC   0(29,R1),=C'ENTER FIRST FORMAT DEFINITION'                       
         XC    KWXACT,KWXACT                                                    
         MVC   KWXACT(09),=C'ADD,REF=1'                                         
         OI    KWXACTH+6,X'80'                                                  
         LA    R2,KWXDATAH                                                      
         ST    R2,ACURSOR                                                       
         B     OKXIT                                                            
*                                                                               
HITENT   L     R1,FULL                                                          
         MVC   0(18,R1),=C'HIT ENTER FOR NEXT'                                  
         XC    KWXACT,KWXACT                                                    
         MVC   KWXACT(12),=C'DISPLAY,NEXT'                                      
HITENT2  OI    KWXACTH+6,X'80'                                                  
         LA    R2,KWXTABH                                                       
         ST    R2,ACURSOR                                                       
         B     OKXIT                                                            
*                                                                               
OKXIT    CLI   ACTION,FOR          DISPLAY BOOK NAME UUUUUU.IIIINNN             
         BE    OKXITX                                                           
         LA    R4,MSGBOOK                                                       
         CLI   SAVMODE,MESSAGE                                                  
         BE    *+8                                                              
         LA    R4,FRMBOOK                                                       
         OC    0(L'MSGBOOK,R4),0(R4)                                            
         BZ    OKXITX                                                           
         MVC   KWXBOOK(4),=C'BOOK'                                              
         LA    R3,KWXBOOK+5                                                     
         LA    R7,KEY                                                           
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),0(R4)                                                
         GOTO1 ADATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,KEY,WORK                 
         CLI   DMCB+8,0                                                         
         BNE   OKXITX                                                           
         LA    R6,CTIDATA                                                       
         SR    R5,R5                                                            
*                                                                               
OKXIT2   CLI   0(R6),0                                                          
         BE    OKXITX                                                           
         CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     OKXIT2                                                           
         USING CTDSCD,R6                                                        
         MVC   0(10,R3),CTDSC                                                   
         DROP  R6                                                               
         LA    R3,5(R3)            BEGIN SCANNING NAME EARLIER                  
***>>>   LA    R3,9(R3)                                                         
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'.'                                                       
         MVC   2(3,R3),3(R4)       INITIALS                                     
         LA    R5,5(R3)                                                         
         CLI   SAVMODE,FORMAT                                                   
         BE    *+14                                                             
         MVC   2(4,R3),2(R4)                                                    
         LA    R5,6(R3)                                                         
         UNPK  0(1,R5),7(1,R4)                                                  
         OI    0(R5),X'F0'                                                      
         MVC   WORK(1),6(R4)                                                    
         MVI   WORK+1,X'0F'                                                     
         UNPK  DUB(3),WORK(2)      NN                                           
         MVC   1(2,R5),DUB                                                      
*                                                                               
OKXITX   LTR   RB,RB               CC = NEQ IF OK                               
         B     EXIT                                                             
         SPACE 1                                                                
*              (C) DATA MANAGER ERRORS                                          
*                                                                               
DMERROR  MVI   FERN,RECNFND                                                     
         TM    DMCB+8,X'10'                                                     
         BO    ERROR                                                            
         MVI   FERN,ENDFILE                                                     
         TM    DMCB+8,X'80'                                                     
         BO    ERROR                                                            
         MVI   FERN,DISKERR                                                     
         XC    DMCB,DMCB                                                        
         XC    MSGBOOK(24),MSGBOOK                                              
ERROR    SR    R0,R0                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
* NESTED INCLUDES                                                               
* CTGENFILE                                                                     
* DMWRKRD                                                                       
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023GEKWX01A  05/01/02'                                      
         END                                                                    
