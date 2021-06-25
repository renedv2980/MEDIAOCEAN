*          DATA SET TAGEN8E    AT LEVEL 024 AS OF 10/04/16                      
*PHASE T7028EB,*                                                                
         TITLE 'T7028E - ADVICE RECORD PRINTING'                                
T7028E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7028E,R7,RR=R2                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         MVC   ADCREC,4(R1)        AREA TO READ ADVICE CAST RECORDS             
         L     R9,ASYSD            R9=SYSTEM STORAGE AREA                       
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         EJECT                                                                  
***********************************************************************         
*      IOAREAS ON ENTRY, AIO   HAS A(ADVICE RECORD)                   *         
*                        AIO2  HAS A(AGENCY RECORD)                   *         
*                        AIO3  USED AS WORK IO                        *         
***********************************************************************         
         SPACE 1                                                                
         ST    R2,RELO             RELOCATION FACTOR                            
         LA    R1,MYSPECS          SET A(SPECS)                                 
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           SET A(HEADINGS ROUTINE)                      
         ST    R1,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'       SET TO START ON NEW PAGE                     
         BAS   RE,SETSPRG          SET RCSUBPRG                                 
         BAS   RE,SETINFO          SET ADVTYPE & NOSNDFLG FOR HDHOOK            
         SPACE 1                                                                
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PRNTD,R3                                                         
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTCLI       CLIENT NAME                
         MVC   PRNTLTAG(L'LTCLI),LTCLI                                          
         MVC   PRNTLEFT(L'TGNAME),TGNAME                                        
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TADVELQ      GET ADVICE DETAILS EL.                       
         BAS   RE,GETEL                                                         
         USING TADVD,R4                                                         
         BNE   PADV5                                                            
         MVC   P+55(L'TADVCLI),TADVCLI                                          
                                                                                
PADV5    BAS   RE,PRNTIT                                                        
         L     R4,AIO                                                           
*                                                                               
         SPACE 1                                                                
         GOTO1 CHAROUT,(R1),TAFNELQ,(X'80',0),TAFNTPRD  PRODUCT NAME            
         MVC   PRNTLTAG(L'LTPRD),LTPRD                                          
         MVC   PRNTLEFT(L'TGNAME),TGNAME                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         GOTO1 (RF),(R1),TAFNELQ,0,TAFNTTTL          COMMERCIAL NAME            
         MVC   PRNTLTAG(L'LTTITLE),LTTITLE                                      
         MVC   PRNTLEFT(L'TGNAME),TGNAME                                        
         SPACE 1                                                                
         MVI   ELCODE,TAVUELQ      GET ADVICE USE DETAILS EL.                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   *+16                                                             
         USING TAVUD,R4                                                         
         MVC   PRNTRTAG(L'LTMED),LTMED  DISPLAY MEDIA                           
         MVC   PRNTRHS(1),TAVUMED                                               
*                                                                               
         MVC   PRNTRTG2(L'LTEXP),LTDUE  DISPLAY DUE                             
*                                                                               
         MVI   ELCODE,TADDELQ      GET DUE DATE ELEMENT                         
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PADV7                                                            
         USING TADDD,R4                                                         
         GOTO1 DATCON,DMCB,(1,TADDDATE),(8,PRNTRHS2)                            
PADV7    MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ      LIFT DETAILS                                 
         BAS   RE,GETEL                                                         
         BNE   PADV8                                                            
         USING TALFD,R4                                                         
         MVC   PRNTLTAG(L'LTLID),LTLID                                          
         MVC   PRNTLEFT(L'TALFLID),TALFLID          LIFT ID                     
         MVC   PRNTLFLT(L'LTLSEC),LTLSEC                                        
         EDIT  (1,TALFSEC),(2,PRNTLSEC),ALIGN=LEFT  LIFT LENGTH                 
PADV8    L     R4,AIO                                                           
         MVI   ELCODE,TADVELQ      GET ADVICE DETAILS EL.                       
         BAS   RE,GETEL                                                         
         USING TADVD,R4                                                         
         BNE   PADV10                                                           
         CLI   TADVSEC,0                                                        
         BE    PADV9                                                            
         MVC   PRNTRTAG(L'LTSEC),LTSEC  DISPLAY LEN                             
         EDIT  (1,TADVSEC),(3,PRNTRHS),ALIGN=LEFT                               
PADV9    OC    TADVEXP,TADVEXP                                                  
         BZ    PADV10                                                           
         MVC   PRNTRTG2(L'LTEXP),LTEXP  DISPLAY EXP                             
         GOTO1 DATCON,DMCB,(1,TADVEXP),(8,PRNTRHS2)                             
PADV10   CLC   P,SPACES                                                         
         BNH   *+8                                                              
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         BAS   RE,DISCONS          AFM CONTRACT NUMBERS                         
         BNE   PADV14                                                           
         MVC   PRNTLTAG(L'LTCON),LTCON                                          
         MVC   PRNTLEFT(L'WORK),WORK                                            
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
PADV14   GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTAUT  AUTH/PO                         
         MVC   PRNTAUTH,TGNAME                                                  
         MVC   PRNTLTAG(L'LTAUTH),LTAUTH                                        
         SPACE 1                                                                
         MVC   PRNTCTAG(L'LTEST),LTEST                                          
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTEST  ESTIMATE                        
         MVC   PRNTEST,TGNAME                                                   
         MVI   ELCODE,TASIELQ                                                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   PRNTEST,=CL16'SPLIT'                                             
         SPACE 1                                                                
         MVI   ELCODE,TAVUELQ      GET ADVICE USE DETAILS EL.                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         BAS   RE,PRNTIT                                                        
         B     PADV20                                                           
         USING TAVUD,R4                                                         
         EDIT  (1,TAVUESPD),(2,PRNTRHS),ZERO=BLANK,ALIGN=LEFT EST PD.           
         MVC   PRNTRTAG(L'LTESPD),LTESPD                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
PADV20   XC    BLOCK(8),BLOCK                                                   
         XC    BLOCK+78(8),BLOCK                                                
         MVI   BLOCK,2*(70+8)                                                   
         GOTO1 CHAROUT,DMCB,TACMELQ,(2,BLOCK),TACMTYPG  COMMENT                 
         BNE   PADV25                                                           
         MVC   PRNTLTAG(L'LTCMNT),LTCMNT                                        
         MVC   PRNTLEFT(70),BLOCK+8                                             
         MVC   PRNTLEFT+L'P(70),BLOCK+78                                        
         BAS   RE,PRNTIT                                                        
         B     PADV30                                                           
         SPACE 1                                                                
PADV25   MVC   PRNTLTAG(L'LTCMNT),LTCMNT                                        
         LA    R5,8                R5=MAX # OF COMMENTS                         
         LA    R6,1                R6=COMMENT COUNTER                           
PADV26   L     R4,AIO                                                           
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL            R4=A(COMMENT ELEMENT)                        
         B     *+8                                                              
PADV27   BRAS  RE,NEXTEL                                                        
         BNE   PADV28                                                           
         USING TAXCD,R4                                                         
         ZIC   RE,TAXCSEQ          IF SEQUENCE # IN ELEMENT IS NOT              
         CR    R6,RE               WHAT WE'RE LOOKING FOR, GET NEXT             
         BNE   PADV27                                                           
         ZIC   RE,TAXCLEN                                                       
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRNTLEFT(0),TAXCCMNT    DISPLAY COMMENT                          
         BAS   RE,PRNTIT                                                        
PADV28   LA    R6,1(R6)            INCREMENT COMMENT FIELD COUNTER              
         BCT   R5,PADV26                                                        
         SPACE                                                                  
PADV30   MVC   PRNTLTAG(12),SPACES                                              
         BAS   RE,PRNTIT           PRINT BLANK LINE                             
         SPACE                                                                  
         MVC   PRNTLTAG(L'LTCMNT),SPACES                                        
         MVI   ELCODE,TADVELQ      GET ADVICE DETAILS EL.                       
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R4                                                         
         CLI   TADVTYPE,TADVTYPS   IF THIS IS A SESSION                         
         BNE   *+12                                                             
         BAS   RE,PRTSESS          PRINT SESSION DETAILS                        
         B     PADV40                                                           
         SPACE 1                                                                
         BAS   RE,PRTREUS          ELSE PRINT REUSE DETAILS                     
         SPACE 1                                                                
PADV40   BAS   RE,PRTSPLT          PRINT ASPLIT SCREEN DETAILS                  
         BAS   RE,PRTDETS          PRINT DETAILS RECORD INDICATOR               
         SPACE 1                                                                
PADVX    B     XIT                                                              
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              ROUTINE TO SET RCSUBPRG FOR SPECS                                
         SPACE 1                                                                
SETSPRG  NTR1                                                                   
         MVI   RCSUBPRG,10         'TALENT ADVICE TITLE'                        
         SPACE 1                                                                
         L     R5,AIO1                                                          
         USING TLDVD,R5                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,TLDVAGY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLAYAGY+L'TLAYAGY-TLAYD),KEYSAVE                             
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         DROP  R5                                                               
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SETSPRGX                                                         
         USING TAAYD,R4                                                         
         TM    TAAYSTA3,TAAYSNCS   IF CAST OKAY TO PULL                         
         BO    SETSPRGX                                                         
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         L     R4,AIO              R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TADVELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SETSPRGX                                                         
         USING TADVD,R4                                                         
         CLI   TADVTYPE,C'S'       AND IF COMPLETION                            
         BNE   SETSPRGX                                                         
         MVI   RCSUBPRG,20         'TALENT ADVICE COMPLETION REPORT'            
         SPACE 1                                                                
SETSPRGX MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE 2                                                                
*        ROUTINE TO SET FLAGS FOR HDHOOK                                        
         SPACE 1                                                                
SETINFO  NTR1                                                                   
         MVI   ADVTYPE,0                                                        
         L     R4,AIO              R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   ADVTYPE,C'G'                                                     
*                                                                               
         MVI   NOSNDFLG,C'N'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',0)                                    
         L     R4,AIO              R4=A(AGENCY RECORD)                          
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SETINFX                                                          
         USING TAAYD,R4                                                         
         MVC   AGYSTAT,TAAYSTAT    SAVE AGENCY STATUS BYTE                      
         TM    TAAYSTA3,TAAYSNSD                                                
         BZ    SETINFX                                                          
         MVI   NOSNDFLG,C'Y'                                                    
         SPACE 1                                                                
SETINFX  MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT SESSION DETAILS                                 
         SPACE 1                                                                
PRTSESS  NTR1                                                                   
         MVI   BYTE,TACSTYPF                           FILM INFO                
         MVC   PRNTLTAG(L'LTFDTE),LTFDTE                                        
         GOTO1 DISSTU,DMCB,PRNTSDTE,PRNTSSTU,PRNTRTG2                           
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         MVI   BYTE,TACSTYPR                           RECORD INFO              
         MVC   PRNTLTAG(L'LTRDTE),LTRDTE                                        
         GOTO1 DISSTU,DMCB,PRNTSDTE,PRNTSSTU,PRNTRTG2                           
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         L     R2,AIO              SAVE AIO                                     
         MVC   AIO,ADCREC          READ ADVICE CAST RECS INTO ADCREC            
         MVC   FULL,DMDSKADD                                                    
         GOTO1 =A(TADCREC),DMCB,(RC),0,RR=RELO   CREAMS AIO3                    
         ST    R2,AIO              RESTORE AIO                                  
         MVC   DMDSKADD,FULL               AND D/A                              
         BNE   XIT                                                              
         SPACE 1                                                                
         BAS   RE,PRTTAVC          PRINT ADVICE CAST DETAIL ELS.                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY A COMMERCIAL CONTRACT ELEMENT                 
         SPACE 1                                                                
DISCONS  NTR1                                                                   
         MVC   WORK,SPACES         INITIALIZE OUTPUT AREA                       
         L     R4,AIO                                                           
         MVI   ELCODE,TACCELQ      SET TO LOOK UP ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TACCD,R4                                                         
         SPACE 1                                                                
         LA    R2,WORK             R2=A(OUTPUT AREA)                            
         ZIC   R3,TACCNCON         R3=N'CONTRACTS TO DISPLAY                    
         LA    R4,TACCCON          R4=A(1ST CONTRACT IN ELEMENT)                
         SPACE 1                                                                
         MVC   0(L'TACCCON,R2),0(R4)  MOVE TO OUTPUT AREA                       
         BAS   RE,SHUFFLE             SHUFFLE POINTER TO NEXT SPOT              
         LA    R4,L'TACCCON(R4)       BUMP TO NEXT                              
         BCT   R3,*-14                                                          
         SPACE 1                                                                
         BCTR  R2,0                BUMP BACK                                    
         MVI   0(R2),C' '          ERASE TRAILING COMMA                         
         SPACE 1                                                                
DCONX    B     YES                                                              
         SPACE 3                                                                
SHUFFLE  DS    0H                  SHUFFLE BACK TO END OF STRING AT R2          
         LA    R2,L'TACCCON-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','          INSERT EMBEDDED COMMA                        
         LA    R2,2(R2)            RETURN R2=A(NEXT SLOT)                       
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY A COMMERCIAL STUDIO ELEMENT                   
         SPACE 1                                                                
*                                  BYTE=ELEMENT TYPE                            
DISSTU   NTR1                                                                   
         MVC   PRNTCTAG(L'LTSTUDIO),LTSTUDIO                                    
         MVC   PRNTRTAG(L'LTCITY),LTCITY                                        
         MVC   PRNTRTAG(L'LTCYST),LTCYST                                        
         SPACE 1                                                                
         LM    R2,R4,0(R1)         R2=A(DATE), R3=A(STUDIO), R4=A(CITY)         
         SPACE 1                                                                
         MVI   ELCODE,TACSELQ      SET TO GET ELEMENT FOR THIS TYPE             
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   DSTUX               NOT FOUND - GET OUT                          
         SPACE 1                                                                
         L     R5,TGELEM           R5=A(COMMERCIAL STUDIO ELEMENT)              
         USING TACSD,R5                                                         
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,(R2))  DATE                          
         MVC   0(L'TACSSTUD,R3),TACSSTUD          STUDIO                        
         MVC   0(L'TACSCITY,R4),TACSCITY          CITY                          
         SPACE 1                                                                
         CLI   TACSLEN,TACSLNQ     IF FILM/MUSIC/RECORD STATE IS SET            
         BL    DSTUX                                                            
         OC    TACSSTAT,SPACES                                                  
         CLC   TACSSTAT,SPACES                                                  
         BE    DSTUX                                                            
         SPACE 1                                                                
         LA    RE,L'TACSCITY+1(R4)                                              
DSTU20   CLI   0(RE),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BNE   DSTU30              IN CITY                                      
         SHI   RE,1                                                             
         B     DSTU20                                                           
                                                                                
DSTU30   MVI   1(RE),C','          THEN ADD COMMA AND STATE                     
         MVC   3(L'TACSSTAT,RE),TACSSTAT                                        
DSTUX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT CAST ADVICE CAST DETAILS ELS.                   
         SPACE 1                                                                
         USING PRNTD,R3            R3=A(PRINT LINE)                             
PRTTAVC  NTR1                                                                   
         BAS   RE,PRNTIT                                                        
         MVC   P+2(L'LTCAST),LTCAST  MOVE OUT CAST HEADINGS                     
         MVC   P2+2(L'LTCAST2),LTCAST2                                          
         TM    TGSYSTAT,TASYSPID                                                
         BZ    PVC1                                                             
         MVC   P1+2(8),=C'PID     '                                             
PVC1     BAS   RE,PRNTIT                                                        
         L     R4,ADCREC           R4=A(ADVICE CAST RECORD(S))                  
         MVI   ELCODE,TAVCELQ      LOOP THROUGH CAST ELEMENTS                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PVC2     BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TAVCD,R4            R4=A(ADVICE CAST DETAILS EL.)                
         SPACE 1                                                                
         TM    TAVCSTAT,TAVCACTL   IF ACTUAL CAST MEMBER                        
         BZ    *+8                                                              
         MVI   PRNTACTL,C'*'       INCIDATE WITH A '*'                          
         SPACE 1                                                                
         OC    TAVCSSN,TAVCSSN                                                  
         BZ    PVC4                                                             
         EDIT  (4,TAVCSSN),(9,PRNTSSN),FILL=0 SOCIAL SECURITY NUMBER            
         TM    TGSYSTAT,TASYSPID                                                
         BZ    PVC4                                                             
         MVC   TGFULL(9),PRNTSSN                                                
         XC    PRNTSSN,PRNTSSN                                                  
         GOTO1 SSNPACK,DMCB,TGFULL,PRNTSSN                                      
         OC    PRNTSSN,SPACES                                                   
         SPACE 1                                                                
PVC4     MVC   PRNTNAME,TAVCNAME              PERFORMER NAME                    
         SPACE 1                                                                
         GOTO1 CATVAL,DMCB,(X'80',TAVCCAT)    CATEGORY                          
         MVC   PRNTCAT,TGCACDE                                                  
         SPACE 1                                                                
         MVC   PRNTCAM,=C'OFF'                ON/OFF CAMERA                     
         TM    TAVCSTAT,TAVCSON                                                 
         BZ    *+10                                                             
         MVC   PRNTCAM,=C'ON '                                                  
         SPACE 1                                                                
         TM    TAVCSTAT,TAVCNEWS                                                
         BO    PVC4A                                                            
         GOTO1 UNIVAL,DMCB,(X'80',TAVCUNI)    UNION                             
         MVC   PRNTUNI,TGUNCDE                                                  
         B     *+10                                                             
PVC4A    MVC   PRNTUNI,TAVCUNI3                                                 
         SPACE 1                                                                
         MVC   PRNTDBL,TAVCDBL                DOUBLES                           
         SPACE 1                                                                
         TM    TAVCSTAT,TAVCLFT               ON LIFT                           
         BZ    *+8                                                              
         MVI   PRNTLFT,C'Y'                                                     
         TM    TAVCSTAT,TAVCOLFT              ONLY ON LIFT                      
         BZ    *+8                                                              
         MVI   PRNTLFT,C'O'                                                     
         SPACE 1                                                                
         LA    R5,PRNTOV1                                                       
         ICM   R6,15,TAVCOV1                                                    
         BNM   PVC05                                                            
         MVI   0(R5),C'%'                                                       
         LA    R5,1(R5)                                                         
         N     R6,=X'7FFFFFFF'                                                  
         ST    R6,TAVCOV1                                                       
PVC05    EDIT  TAVCOV1,(6,0(R5)),2,ALIGN=LEFT OVERSCALE RATE 1                  
         LR    R1,R0                                                            
         LA    R1,PRNTOV1-3(R1)                                                 
         CLC   0(3,R1),=C'.00'     DON'T DISPLAY FRACTION IF ZERO               
         BNE   *+10                                                             
         MVC   0(3,R1),SPACES                                                   
         SPACE 1                                                                
         EDIT  (4,TAVCOV2),(6,PRNTOV2),2,ALIGN=LEFT OVERSCALE RATE 2            
         LR    R1,R0                                                            
         LA    R1,PRNTOV2-3(R1)                                                 
         CLC   0(3,R1),=C'.00'     DON'T DISPLAY FRACTION IF ZERO               
         BNE   *+10                                                             
         MVC   0(3,R1),SPACES                                                   
         SPACE 1                                                                
         TM    TAVCSTAT,TAVCDEL    IF ACTUAL CAST MEMBER DELETED                
         BZ    *+10                                                             
         MVC   PRNTTDEL,LTDEL      SHOW AS '*DELETED*'                          
         SPACE 1                                                                
         LHI   RE,TAVCLNQ                                                       
         LA    RF,TAVCCMT                                                       
         TM    TAVCSTAT,TAVCNEWS                                                
         BO    *+12                                                             
         SHI   RE,3                                                             
         SHI   RE,3                                                             
         SPACE 1                                                                
         STC   RE,BYTE                                                          
         CLC   TAVCLEN,BYTE        THERE MAY BE A COMMENT                       
         BNH   PVCX                                                             
         MVC   PRNTCMTT,LTPCMT                                                  
         ZIC   R1,TAVCLEN          SET FOR EXECUTED MOVE                        
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRNTCMT(0),0(RF)                                                 
         SPACE 1                                                                
PVCX     MVI   SPACING,2           PRINT THE LINE                               
         BAS   RE,PRNTIT                                                        
         MVI   ELCODE,TAVCELQ      LOOP THROUGH CAST ELEMENTS                   
         B     PVC2                LOOK FOR ANOTHER ELEMENT                     
         EJECT                                                                  
*              ROUTINE TO PRINT REUSE DETAILS                                   
         SPACE 1                                                                
PRTREUS  NTR1                                                                   
         MVI   REGOTH,C'N'         SEE IF COMMENT ELEMENT                       
         MVI   ELCODE,TACMELQ      EXISTS FOR REGULAR USE                       
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPU))                                     
         BNE   *+8                                                              
         MVI   REGOTH,C'Y'                                                      
         SPACE 1                                                                
         MVI   MUSOTH,C'N'         SEE IF COMMENT ELEMENT                       
         MVI   ELCODE,TACMELQ      EXISTS FOR MUSIC USE                         
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPM))                                     
         BNE   *+8                                                              
         MVI   MUSOTH,C'Y'                                                      
         SPACE 1                                                                
         L     R4,AIO              R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAVUELQ      GET ADVICE USE DETAILS EL.                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAVUD,R4            R4=A(ADVICE USE DETAILS EL.)                 
         SPACE 1                                                                
         LA    R5,TAVUUSE          DEFAULT TO REGULAR USE                       
         LA    R6,TAVUTYPE                    REGULAR TYPE                      
         MVC   CYCSTRT(6),TAVUCYC             REGULAR CYCLE DATES               
         CLI   TAVULEN,27          IF INTERMEDIATE ADVICE                       
         BNE   PRUS3                        ONLY HAVE CYCLE START               
         XC    CYCSTRT+3(3),CYCSTRT+3                                           
         CLI   TAVUUSE,UFGR                                                     
         BNE   PRUS3                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TADVELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PRUS2                                                            
         USING TADVD,R4                                                         
         OC    TADVEXP,TADVEXP                                                  
         BZ    PRUS2                                                            
         MVC   CYCSTRT+3(3),TADVEXP                                             
PRUS2    L     R4,AIO                                                           
         MVI   ELCODE,TAVUELQ                                                   
         BAS   RE,GETEL                                                         
         USING TAVUD,R4                                                         
         SPACE 1                                                                
PRUS3    OC    TAVUCYC,TAVUCYC     IF NO REGULAR CYCLE                          
         BNZ   PRUS4                                                            
         LA    R5,TAVUUSE2         RESET TO MUSIC USE                           
         LA    R6,TAVUTYP2                  MUSIC TYPE                          
         MVC   CYCSTRT(6),TAVUMCYS          MUSIC CYCLE START                   
         CLI   TAVULEN,27          IF INTERMEDIATE ADVICE                       
         BNE   *+10                         ONLY HAVE CYCLE START               
         XC    CYCSTRT+3(3),CYCSTRT+3                                           
         SPACE 1                                                                
PRUS4    BRAS  RE,CYCDTS                      PRINT CYCLE DATES                 
         SPACE 1                                                                
         MVC   PRNTLTAG(L'LTUSE),LTUSE        PRINT USE HEADER                  
         SPACE 1                                                                
         CLI   0(R5),0                        IF USE IS OTHER                   
         BNE   PRUS4AC                                                          
         MVC   PRNTLEFT,SPACES                DISPLAY OTHER                     
         MVC   PRNTLEFT(5),=C'OTHER'                                            
         CLI   MUSOTH,C'Y'                                                      
         BNE   *+10                           OR OTHER MUSIC                    
         MVC   PRNTLEFT(11),=C'OTHER MUSIC'                                     
         BAS   RE,PRNTIT                                                        
         XC    BLOCK(8),BLOCK            DISPLAY OTHER COMMENT                  
         MVI   BLOCK,40+8                                                       
         CLI   MUSOTH,C'Y'                                                      
         BE    PRUS6                                                            
         GOTO1 CHAROUT,DMCB,TACMELQ,(X'80',BLOCK),TACMTYPU                      
         B     PRUS7                                                            
PRUS6    GOTO1 CHAROUT,DMCB,TACMELQ,(X'80',BLOCK),TACMTYPM                      
PRUS7    MVC   PRNTLEFT(30),BLOCK+8                                             
         BAS   RE,PRNTIT                                                        
         B     PRUS10                                                           
         SPACE 1                                                                
PRUS4AC  GOTO1 USEVAL,DMCB,(X'80',(R5)),(R6)  GET USE TYPE                      
         MVC   PRNTLEFT(L'TGUSNAME),TGUSNAME  PRINT ITS NAME                    
         SPACE 1                                                                
         CLI   0(R5),UITN          IF USE IS ITN                                
         BNE   *+8                                                              
         BRAS  RE,PRNTITN                                                       
         SPACE 1                                                                
         CLI   0(R5),UMUS                                                       
         BNE   PRUS4ACA                                                         
         CLI   0(R6),UMUSDSH                                                    
         BNL   PRUS4ACA                                                         
         CLI   TAVULEN,27                                                       
         BE    PRUS4ACA                                                         
         LA    RE,PRNTLEFT                                                      
         SR    RF,RF                                                            
PRUS4ACB CLI   0(RE),C' '                                                       
         BE    PRUS4ACC                                                         
         LA    RE,1(RE)                                                         
         B     PRUS4ACB                                                         
PRUS4ACC AHI   RF,1                                                             
         CHI   RF,2                                                             
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     PRUS4ACB                                                         
         MVC   1(2,RE),TAVUWKS                                                  
         MVC   3(10,RE),SPACES                                                  
         LA    RF,TAVUUSE                                                       
         CR    RF,R5                                                            
         BE    *+10                                                             
         MVC   1(2,RE),TAVUMWKS                                                 
         OC    PRNTLEFT,SPACES                                                  
         CLI   2(RE),C' '                                                       
         BNE   *+8                                                              
         AHI   RE,-1                                                            
         MVC   3(2,RE),=C'WK'                                                   
PRUS4ACA CLI   0(R5),UWSP                     IF USE IS WILDSPOT                
         BNE   PRUS4AA                                                          
         LA    RE,PRNTLEFT                                                      
PRUS4AB  CLC   0(2,RE),SPACES                                                   
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     PRUS4AB                                                          
         CLI   0(R6),UWSP13W                                                    
         BE    *+12                                                             
         CLI   0(R6),UWSPU13W                                                   
         BNE   PRUS4AA                                                          
         CLI   TAVUWKS,C'8'                                                     
         BE    PRUS4AA                                                          
         CLI   TAVUWKS+1,C'8'                                                   
         BE    PRUS4AA                                                          
         MVC   1(4,RE),=C'13WK'               OR 13 WEEKS                       
         SPACE 1                                                                
PRUS4AA  CLI   0(R5),UWSM          IF USE IS WILDSPOT COMBINED                  
         BE    *+12                                                             
         CLI   0(R5),UNET          OR ACTRA/UDA TV NWK                          
         BNE   PRUS5A                                                           
         TM    TGUSTYST,UPGRADE            AND AN UPGRADE                       
         BZ    PRUS5A                                                           
         MVC   PRNTLEFT+17(7),=C'UPGRADE'  INDICATE SO                          
         SPACE 1                                                                
PRUS5A   CLI   0(R5),UFGM          IF USE IS FOREIGN MUSIC                      
         BNE   *+24                                                             
         CLI   0(R6),UFGMNE12      AND TYPE IS NOT EUROPE (12M)                 
         BE    *+16                                                             
         CLI   0(R6),UFGMNE24      OR NOT EUROPE (24M)                          
         BNE   *+8                                                              
         MVI   PRNTLEFT+16,C'M'    ADD M FOR MONTHS AT END OF DESCR             
         SPACE 1                                                                
         BAS   RE,PRNTIT                                                        
         CLI   0(R5),UWSP          IF USE IS WILDSPOT                           
         BNE   *+8                                                              
         BAS   RE,PRNTWSP          PRINT WSP DETAILS                            
         SPACE 1                                                                
         CLI   0(R5),USCB          IF USE IS SPANISH CABLE                      
         BE    PRUS9                                                            
         CLI   0(R5),USWS          OR SPANISH WILDSPOT                          
         BE    PRUS9                                                            
         CLI   0(R5),USNW          OR SPA/NETWORK COMBINED                      
         BE    PRUS9                                                            
         CLI   0(R5),UCBL          OR CABLE '91                                 
         BE    PRUS9                                                            
         CLI   0(R5),ULCB          OR LOCAL CABLE                               
         BNE   *+8                                                              
PRUS9    BAS   RE,PRNTCBL          PRINT DETAILS                                
         SPACE 1                                                                
         CLI   0(R5),URRN          IF USE IS RADIO REGIONAL NETWORK             
         BE    *+12                                                             
         CLI   0(R5),UFGR          OR FOREIGN RADIO                             
         BNE   *+8                                                              
         BAS   RE,PRNTRRN          PRINT DETAILS                                
         SPACE 1                                                                
         CLI   0(R5),UNET          IF USE IS ACTRA/UDA NETWORK                  
         BE    PRUS9A                                                           
         CLI   0(R5),UWSC          OR WILDSPOT CANADIAN                         
         BE    PRUS9A                                                           
         CLI   0(R5),UADC          OR ADDENDUM COMBINED SESSION                 
         BE    PRUS9A                                                           
         CLI   0(R5),UADW          OR ADDENDUM WILDSPOT                         
         BE    PRUS9A                                                           
         CLI   0(R5),UWSM          OR WILDSPOT COMBINED                         
         BNE   *+8                                                              
PRUS9A   BAS   RE,PRNTNET          PRINT  DETAILS                               
         SPACE 1                                                                
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTFEE))                                     
         BNE   PRUS10                                                           
         L     R1,TGELEM                                                        
         USING TANUD,R1                                                         
         ZIC   RE,TANULEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRNTLTAG+23(0),TANUMBER                                          
         MVC   PRNTLTAG(22),=C'Number of Session Fees'                          
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
PRUS10   BAS   RE,PRTINV                                                        
         SPACE 1                                                                
         LA    RE,TAVUUSE2                                                      
         CR    R5,RE                                                            
         BE    PRUS13                                                           
         SPACE 1                                                                
         CLI   TAVUUSE2,0                    IF SECOND USE IS OTHER             
         BNE   PRUS10C                                                          
         CLI   TAVUUSE,0                                                        
         BE    PRUS12                                                           
         CLI   MUSOTH,C'Y'                                                      
         BNE   PRUS12                                                           
         MVC   PRNTLTAG(L'LTUSE2),LTUSE2                                        
         MVC   PRNTLEFT,SPACES                                                  
         MVC   PRNTLEFT(11),=C'OTHER MUSIC'                                     
PRUS10C  MVC   CYCSTRT(6),TAVUMCYS    IF NEW ADVICE WE HAVE FULL                
         CLI   TAVULEN,TAVULNQ        CYCLE DATES                               
         BE    PRUS10D                                                          
         CLI   TAVULEN,27             IF INTERMEDIATE ADVICE WE                 
         BNE   *+14                   HAVE CYCLE START                          
         XC    CYCSTRT+3(3),CYCSTRT+3                                           
         B     *+10                   IF OLD ADVICE WE HAVE NO                  
         XC    CYCSTRT(6),CYCSTRT     CYCLE DATES                               
         SPACE 1                                                                
PRUS10D  OC    CYCSTRT(6),CYCSTRT                                               
         BNZ   *+10                                                             
         MVC   CYCSTRT(6),TAVUCYC                                               
         SPACE 1                                                                
         LA    R5,TAVUUSE2                                                      
         LA    R6,TAVUTYP2                                                      
         BRAS  RE,CYCDTS                                                        
         CLI   MUSOTH,C'Y'                                                      
         BNE   PRUS11                                                           
         BAS   RE,PRNTIT                                                        
         XC    BLOCK(8),BLOCK         PRINT OUT OTHER COMMENT                   
         MVI   BLOCK,40+8                                                       
         GOTO1 CHAROUT,DMCB,TACMELQ,(X'80',BLOCK),TACMTYPM                      
         MVC   PRNTLEFT(30),BLOCK+8                                             
         B     PRUS11A                                                          
         SPACE 1                                                                
PRUS11   MVC   PRNTLTAG(L'LTUSE2),LTUSE2                                        
         GOTO1 USEVAL,DMCB,(X'80',TAVUUSE2),TAVUTYP2                            
         MVC   PRNTLEFT(L'TGUSNAME),TGUSNAME  PRINT SECOND USE NAME             
         SPACE 1                                                                
         CLI   TAVUUSE2,UFGM          IF 2ND USE IS FOREIGN MUSIC               
         BNE   *+24                                                             
         CLI   TAVUTYP2,UFGMNE12      AND TYPE IS NOT EUROPE (12M)              
         BE    *+16                                                             
         CLI   TAVUTYP2,UFGMNE24      OR NOT EUROPE (24M)                       
         BNE   *+8                                                              
         MVI   PRNTLEFT+16,C'M'       ADD M FOR MONTHS TO DESCRIP               
PRUS11A  BAS   RE,PRNTIT                                                        
         BAS   RE,PRTINV                                                        
PRUS12   BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
PRUS13   MVC   PRNTLTAG(L'LTABSS),LTABSS                APPLY SESSION           
         MVC   PRNTLTAG+L'LTABSS+1(1),TAVUABSS                                  
         MVC   PRNTCTAG(L'LTAHLD),LTAHLD                APPLY HOLD              
         MVC   PRNTCTAG+L'LTAHLD+1(1),TAVUAHLD                                  
         GOTO1 CHAROUT,DMCB,TACMELQ,(X'80',0),TACMTYPA  APPLY COMMENT           
         MVC   PRNTRTAG(L'TGNAME),TGNAME                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         CLI   TAVUUSE,UCLA        FOR CLA,LNA,LNN OR LNC USES                  
         BE    PRUS16                                                           
         CLI   TAVUUSE,ULNA                                                     
         BE    PRUS16                                                           
         CLI   TAVUUSE,ULNN                                                     
         BE    PRUS16                                                           
         CLI   TAVUUSE,ULNF                                                     
         BE    PRUS16                                                           
         CLI   TAVUUSE,UPAX                                                     
         BE    PRUS16                                                           
         CLI   TAVUUSE,ULNC                                                     
         BNE   PRUS17                                                           
PRUS16   BAS   RE,PRNTCLA          PRINT CLA PROGRAM DETAILS                    
         SPACE 1                                                                
PRUS17   BAS   RE,PRNTCBLD         PRINT CNET/MKT/CSYS                          
         BRAS  RE,PRNTINMD         PRINT INTERNET/NEW MEDIA                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE LOOKS UP USE CODE ENTRY                                  
         SPACE 1                                                                
*                                  P1=A(USE EQUATE), P2=A(USE TYPE EQU)         
GETUSE   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         CLI   0(R2),0             IF USE DEFINED                               
         BE    NO                                                               
         LA    RF,X'80'            SET TYPE IS EQUATE                           
         CLI   0(R3),0             IF TYPE NOT DEFINED                          
         BNE   *+8                                                              
         LA    RF,X'C0'            SET TO IGNORE                                
         GOTO1 USEVAL,DMCB,((RF),(R2)),(R3)                                     
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT WSP DETAILS                                     
         SPACE 1                                                                
         USING TAVUD,R4            R4=A(ADVICE USE DETAILS EL.)                 
PRNTWSP  NTR1                                                                   
         MVC   PRNTLTAG(L'LTMAJ),LTMAJ                                          
         GOTO1 MAJVAL,DMCB,(X'80',TAVUMAJ)                                      
         MVC   PRNTLEFT(L'TGMACHAR),TGMACHAR                                    
         SPACE 1                                                                
         MVC   PRNTCTAG(L'LTUNT),LTUNT                                          
         OC    TAVUUNT,TAVUUNT                                                  
         BNZ   PWSP05                                                           
         MVC   SVUSE,TAVUUSE                                                    
         BRAS  RE,CCBLUNIT                                                      
         MVC   TAVUUNT,UNITCALC+2                                               
PWSP05   EDIT  (2,TAVUUNT),(4,PRNTCTR),ALIGN=LEFT,ZERO=NOBLANK                  
         GOTO1 RECVAL,DMCB,TLDVCDQ,(X'30',0)                                    
         SPACE 1                                                                
PWSP06   L     R4,AIO              NOW LOOK FOR UPGRADE DETAILS                 
         MVI   ELCODE,TAUPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PWSP10                                                           
         MVC   PRNTLTAG(L'LTCURR),LTCURR  FOUND IT - CHANGE LITERAL             
         MVC   P2,P                MOVE CURRENT INFO TO P2                      
         MVC   P,SPACES            AND SET TO PRINT PREV. INFO IN P1            
         SPACE 1                                                                
         USING TAUPD,R4                                                         
         MVC   PRNTLTAG(L'LTPREV),LTPREV                                        
         GOTO1 MAJVAL,DMCB,(X'80',TAUPIMAJ)                                     
         MVC   PRNTLEFT(L'TGMACHAR),TGMACHAR                                    
         SPACE 1                                                                
         MVC   PRNTCTAG(L'LTUNT),LTUNT                                          
         EDIT  (2,TAUPIUNT),(4,PRNTCTR),ALIGN=LEFT,ZERO=NOBLANK                 
         SPACE 1                                                                
PWSP10   MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT CBL DETAILS                                     
         SPACE 1                                                                
         USING TAVUD,R4            R4=A(ADVICE USE DETAILS EL.)                 
PRNTCBL  NTR1                                                                   
         MVC   SVUSE,TAVUUSE                                                    
         SPACE 1                                                                
         CLI   TAVUUSE,ULCB                                                     
         BE    CBL10                                                            
         SPACE 1                                                                
         MVC   PRNTLTAG(L'LTUNT),LTUNT                                          
         SPACE 1                                                                
CBL05    OC    TAVUUNT,TAVUUNT        IF NO UNITS DEFINED                       
         BNZ   *+14                                                             
         MVC   PRNTLEFT(5),=C'LOCAL'  MIGHT BE LOCAL                            
         B     CBL10                                                            
         EDIT  (2,TAVUUNT),(4,PRNTLEFT),ALIGN=LEFT,ZERO=NOBLANK                 
         SPACE 1                                                                
CBL10    BRAS  RE,CCBLUNIT                                                      
         OC    UNITCALC,UNITCALC                                                
         BZ    CBL50                                                            
         SPACE 1                                                                
CBL20    CLI   TAVUUSE,ULCB                                                     
         BNE   CBL40                                                            
         MVC   PRNTLTAG(L'LTSUB),LTSUB                                          
         MVC   PRNTLEFT(5),SPACES                                               
         CLC   UNITCALC,=F'1000000'                                             
         BNE   CBL30                                                            
         MVC   PRNTLEFT(4),=C'1MIL'                                             
         B     CBL50                                                            
CBL30    CLC   UNITCALC,=F'1000000'                                             
         BNH   CBL40                                                            
         MVC   PRNTLEFT(5),=C'>1MIL'                                            
         B     CBL50                                                            
CBL40    EDIT  UNITCALC,(6,PRNTLEFT),ALIGN=LEFT                                 
         DROP  R4                                                               
         SPACE 1                                                                
CBL50    GOTO1 RECVAL,DMCB,TLDVCDQ,(X'30',0)                                    
         SPACE 1                                                                
CBL60    L     R4,AIO              NOW LOOK FOR UPGRADE DETAILS                 
         MVI   ELCODE,TAUPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PCBLX                                                            
         MVC   WORK(60),PRNTLTAG                                                
         MVC   PRNTLTAG(L'LTCURR),LTCURR  FOUND IT - ADD LITERAL                
         MVC   PRNTLEFT(60),WORK                                                
         MVC   P2,P                MOVE CURRENT INFO TO P2                      
         MVC   P,SPACES            AND SET TO PRINT PREV. INFO IN P1            
         SPACE 1                                                                
         USING TAUPD,R4                                                         
         MVC   PRNTLTAG(L'LTPREV),LTPREV                                        
         MVC   PRNTLEFT(L'LTUNT),LTUNT                                          
         LA    RE,PRNTLEFT+PRNTLEFT-PRNTLTAG                                    
         CLI   SVUSE,ULCB                                                       
         BE    PCBL70                                                           
         EDIT  TAUPIUNT,(4,(RE)),ALIGN=LEFT,ZERO=NOBLANK                        
         B     PCBLX                                                            
         SPACE 1                                                                
PCBL70   MVC   PRNTLEFT(L'LTSUB),LTSUB                                          
         CLC   TAUPISUB,=F'1000000'                                             
         BNE   PCBL71                                                           
         MVC   0(4,RE),=C'1MIL'                                                 
         B     PCBLX                                                            
PCBL71   CLC   TAUPISUB,=F'1000000'                                             
         BNH   PCBL72                                                           
         MVC   0(5,RE),=C'>1MIL'                                                
         B     PCBLX                                                            
PCBL72   EDIT  TAUPISUB,(6,(RE)),ALIGN=LEFT,ZERO=NOBLANK                        
         SPACE 1                                                                
PCBLX    MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT CNET/MKT/CSYS DETAILS                           
         SPACE 1                                                                
         USING TAVUD,R4                                                         
PRNTCBLD NTR1                                                                   
         LA    R3,LTCBLD           SET UP COLUMN HEADERS                        
         CLI   TAVUUSE,UCBL                                                     
         BE    PCBLD05                                                          
         CLI   TAVUUSE,USCB                                                     
         BE    PCBLD05                                                          
         LA    R3,LTLCBD                                                        
         CLI   TAVUUSE,ULCB                                                     
         BE    PCBLD05                                                          
         LA    R3,LTMKTD                                                        
         SPACE 1                                                                
PCBLD05  L     R4,AIO                                                           
         MVI   ELCODE,TAMTELQ                                                   
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         BNE   PCBLDX                                                           
         SPACE 1                                                                
         MVC   P+1(L'LTDATE),LTDATE       IF IT EXIST                           
         MVC   P2+1(L'LTDATE2),LTDATE2    PRINT OUT DATE HEADING                
         MVC   P+10(L'LTCBLD),0(R3)                                             
         MVC   P2+10(L'LTCBLD2),LTCBLD2   AND CNET/MKT/CSYS HEADING             
         SPACE 1                                                                
         MVC   P+22(L'LTDATE),LTDATE                                            
         MVC   P2+22(L'LTDATE2),LTDATE2                                         
         MVC   P+31(L'LTCBLD),0(R3)                                             
         MVC   P2+31(L'LTCBLD2),LTCBLD2                                         
         SPACE 1                                                                
         MVC   P+43(L'LTDATE),LTDATE                                            
         MVC   P2+43(L'LTDATE2),LTDATE2                                         
         MVC   P+52(L'LTCBLD),0(R3)                                             
         MVC   P2+52(L'LTCBLD2),LTCBLD2                                         
         SPACE 1                                                                
         MVC   P+64(L'LTDATE),LTDATE                                            
         MVC   P2+64(L'LTDATE2),LTDATE2                                         
         MVC   P+73(L'LTCBLD),0(R3)                                             
         MVC   P2+73(L'LTCBLD2),LTCBLD2                                         
         SPACE 1                                                                
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         LA    R3,P                R3=A(PRINT LINE)                             
         LA    R2,4                SET FOR 4-UP                                 
         B     PCBLD20                                                          
         SPACE 1                                                                
PCBLD10  BAS   RE,NEXTEL                                                        
         BNE   PCBLD40                                                          
         SPACE 1                                                                
         USING TAMTD,R4                                                         
PCBLD20  MVC   PCBLCODE,TAMTCODE                                                
         OC    TAMTCYCS,TAMTCYCS                                                
         BZ    PCBLD30                                                          
         GOTO1 DATCON,DMCB,(1,TAMTCYCS),(8,PCBLDATE)                            
         SPACE 1                                                                
PCBLD30  LA    R3,PCBLDRHS         POINT TO RHS                                 
         BCT   R2,PCBLD10          AND CONTINUE IF OK                           
         SPACE 1                                                                
         BAS   RE,PRNTIT           ALREADY ON RHS SO PRINT LINE                 
         LA    R3,P                AND RESET TO LHS                             
         LA    R2,4                RE-SET FOR 4-UP                              
         B     PCBLD10             CONTINUE                                     
         SPACE 1                                                                
PCBLD40  CLC   P,SPACES            IF ANYTHING LEFT TO PRINT                    
         BE    *+8                                                              
         BAS   RE,PRNTIT           PRINT IT                                     
PCBLDX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT RADIO REGIONAL NETWORK DETAILS                  
         SPACE 1                                                                
         USING TAVUD,R4            R4=A(ADVICE USE DETAILS EL.)                 
PRNTRRN  NTR1                                                                   
         MVC   PRNTLTAG(L'LTAREA),LTAREA                                        
         CLI   0(R5),UFGR                                                       
         BE    *+10                                                             
         MVC   PRNTLTAG(L'LTMAJ),LTMAJ                                          
         GOTO1 MAJVAL,DMCB,(X'80',TAVUMAJ)                                      
         MVC   PRNTLEFT(L'TGMACHAR),TGMACHAR                                    
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT ACTRA/UDA TV NETWORK DETAILS                    
         SPACE 1                                                                
         USING TAVUD,R4            R4=A(ADVICE USE DETAILS EL.)                 
PRNTNET  NTR1                                                                   
         MVC   PRNTCTAG(L'LTUNT),LTUNT                                          
         OC    TAVUUNT,TAVUUNT                                                  
         BNZ   PNET10                                                           
         MVC   SVUSE,TAVUUSE                                                    
         BRAS  RE,CCBLUNIT                                                      
         MVC   TAVUUNT,UNITCALC+2                                               
PNET10   EDIT  (2,TAVUUNT),(4,PRNTCTR),ALIGN=LEFT,ZERO=NOBLANK                  
         GOTO1 RECVAL,DMCB,TLDVCDQ,(X'30',0)                                    
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT CLASS A PROGRAM DETAILS                         
         SPACE 1                                                                
PRNTCLA  NTR1                                                                   
         MVC   P+1(L'LTCLA),LTCLA  PRINT CLA DETAIL HEADINGS                    
         MVC   P2+1(L'LTCLA2),LTCLA2                                            
         MVC   P+41(L'LTCLA),LTCLA                                              
         MVC   P2+41(L'LTCLA2),LTCLA2                                           
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         LA    R3,P                R3=A(PRINT LINE)                             
         LA    R2,2                SET FOR 2-UP                                 
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ                                                   
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     *+8                                                              
PCLA2    BAS   RE,NEXTEL                                                        
         BNE   PCLAX                                                            
         USING TANPD,R4            R4=A(CLA PROGRAM DETAILS EL.)                
         SPACE 1                                                                
         EDIT  TANPUSEN,(4,PCLAUSEN),ALIGN=LEFT USE NUMBER                      
         SPACE 1                                                                
         MVC   PCLADATE(3),TANPDATE USE DATE                                    
         CLC   TANPDATE,=C'TBA'                                                 
         BE    PCLA6                                                            
         GOTO1 DATCON,DMCB,(1,TANPDATE),(8,PCLADATE)                            
         SPACE 1                                                                
PCLA6    MVC   PCLAPNME,TANPPNME   PROGRAM NAME                                 
         SPACE 1                                                                
         MVC   PCLALFT,TANPLFT     LIFT                                         
         SPACE 1                                                                
         MVC   PCLANWK,TANPNWK     NETWORK                                      
         SPACE 1                                                                
         LA    R3,PCLARHS          POINT TO RHS                                 
         BCT   R2,PCLA2            AND CONTINUE IF OK                           
         SPACE 1                                                                
         BAS   RE,PRNTIT           ALREADY ON RHS SO PRINT LINE                 
         LA    R3,P                AND RESET TO LHS                             
         LA    R2,2                RE-SET FOR 2-UP                              
         B     PCLA2               CONTINUE                                     
         SPACE 1                                                                
PCLAX    CLC   P,SPACES            IF ANYTHING LEFT TO PRINT                    
         BE    *+8                                                              
         BAS   RE,PRNTIT           PRINT IT                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ASPLIT SCREEN INFO                                    
PRTSPLT  NTR1                                                                   
         LA    R3,P                R3=A(PRINT LINE)                             
         L     R4,AIO                                                           
         MVI   ELCODE,TASIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PRTSPX                                                           
         BAS   RE,PRNTIT           SKIP A LINE                                  
         MVC   P+1(L'LTSPLT),LTSPLT                                             
         MVC   P2+1(L'LTSPLT2),LTSPLT2                                          
         BAS   RE,PRNTIT                                                        
         B     PRTSP20                                                          
*                                                                               
PRTSP10  BAS   RE,NEXTEL                                                        
         BNE   PRTSPX                                                           
         USING TASID,R4                                                         
*                                                                               
PRTSP20  BAS   RE,PRTEST           PRINT ESTIMATE NUMBER                        
         BAS   RE,PRTPCT           PRINT PERCENT                                
*                                                                               
         BAS   RE,PRNTIT           PRINT IT                                     
         B     PRTSP10                                                          
*                                                                               
PRTSPX   B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE PRINTS ESTIMATE NUMBER                                   
PRTEST   NTR1                                                                   
         LA    R2,PSPLTEST                                                      
         ZIC   RE,TASILEN                                                       
         SH    RE,=Y(TASILNQ+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TASIEST     PRINT ESTIMATE NUMBER                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS PERCENTAGE                                        
PRTPCT   NTR1                                                                   
         LA    R2,PSPLTPCT                                                      
         EDIT  TASIPCT3,(7,0(R2)),4,ALIGN=LEFT                                  
         AR    R2,R0               + LENGTH OF EDIT                             
         SH    R2,=H'4'            PT R2 TO DECIMAL PLACES                      
         LA    R1,3                CHECK  DECIMALS FOR ZERO                     
PRTPCT5  EX    R1,CKZERO                                                        
         BE    PRTPCT8                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,PRTPCT5                                                       
         B     PRTPCTX             NO ZEROIES IN DECIMAL                        
*                                                                               
PRTPCT8  CH    R1,=H'3'            IF ALL DECIMAL PLACES ARE ZERO               
         BNE   PRTPCT9                                                          
         LA    R1,1(R1)            CLEAR DEIMAL POINT TOO                       
         BCTR  R2,0                                                             
PRTPCT9  EX    R1,MVCSPC           SET TO SPACES                                
PRTPCTX  B     XIT                                                              
         SPACE 2                                                                
CKZERO   CLC   0(0,R2),=C'0000'    CHECK IF ZEROES                              
MVCSPC   MVC   0(0,R2),SPACES      SET ZEROES TO SPACES                         
         EJECT                                                                  
*              ROUTINE TO PRINT DETAILS RECORD INDICATOR                        
PRTDETS  NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'C0',TYPEV)                                
         GOTO1 HIGH                                                             
         CLC   KEY(TLCMLEV-TLCMKEY),KEYSAVE                                     
         BNE   PDX                                                              
         SPACE 1                                                                
         LA    R3,P                R3=A(PRINT LINE)                             
         BAS   RE,PRNTIT           SKIP A LINE                                  
         SPACE 1                                                                
         MVC   P+1(L'LTDETAIL),LTDETAIL                                         
         BAS   RE,PRNTIT                                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
PDX      GOTO1 RECVAL,DMCB,TLDVCDQ,(X'20',0)                                    
         B     XIT                                                              
         SPACE 2                                                                
TYPEV    DC    C'V'                                                             
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS OUT ASSIGNED INVOICE NUMBER                       
*              ON ENTRY ... R4=A(ADVICE USE DETAILS ELEMENT)                    
*                           R5=A(REUSE OR MUSIC USE BEING PROCESSED)            
         SPACE 1                                                                
PRTINV   NTR1                                                                   
         USING TAVUD,R4                                                         
         LHI   R1,1              IF PROCESSING REUSE, SET R1=1                  
         LA    RE,TAVUUSE                                                       
         CR    R5,RE                                                            
         BE    PRTINV10                                                         
         LHI   R1,2              IF PROCESSING MUSIC, SET R1=2                  
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAAID,R4                                                         
PRTINV10 L     R4,AIO                                                           
         MVI   ELCODE,TAAIELQ    GET ADVICE/INVOICE ASSIGN ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   PRTINVX                                                          
         SPACE 1                                                                
         MVC   PRNTLTAG(L'LTINV),LTINV                                          
         SPACE 1                                                                
         LA    R2,TAAIINUM                                                      
         CHI   R1,1                                                             
         BE    PRTINV20                                                         
         LA    R2,TAAIINU2                                                      
         SPACE 1                                                                
PRTINV20 MVC   PRNTLTAG+L'LTINV+2(L'TAAIINUM),0(R2)                             
         SPACE 1                                                                
         CHI   R1,1                                                             
         BNE   PRTINV30                                                         
         CLI   TAAILEN,TAAIILNQ                                                 
         BE    PRTINV30                                                         
         MVI   PRNTLTAG+L'LTINV+L'TAAIINUM+2,C'-'                               
         MVC   PRNTLTAG+L'LTINV+L'TAAIINUM+3(L'TAAIINUX),TAAIINUX               
PRTINV30 BAS   RE,PRNTIT                                                        
PRTINVX  B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
         L     R3,AIO              R3=A(ADVICE RECORD)                          
         USING TLDVD,R3                                                         
         ZIC   R2,ELCODE           SAVE CURRENT ELCODE                          
*                                                                               
         LR    R4,R3                                                            
         MVI   ELCODE,TADVELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   HDHOOK5                                                          
         USING TADVD,R4                                                         
*                                                                               
         CLI   ADVTYPE,C'G'        IF GENERATED ADVICE                          
         BNE   HDHOOK2                                                          
         CLI   NOSNDFLG,C'Y'       AND DON'T HAVE TO SEND ADVICES               
         BNE   HDHOOK2                                                          
         TM    TADVSTAT,TADVSPAY   GIVE MESSAGE IF NOT PAID                     
         BO    HDHOOK5                                                          
         MVC   H3+25(L'LTNTPAID),LTNTPAID                                       
         B     HDHOOK5                                                          
HDHOOK2  TM    TADVSTAT,TADVSSNT   ELSE, GIVE MESSAGE IF NOT SENT               
         BO    HDHOOK5                                                          
         MVC   H3+22(L'LTNTSENT),LTNTSENT                                       
*                                                                               
HDHOOK5  MVC   H4+11(6),TLDVAGY    AGENCY                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING TLAYD,R5                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,TLDVAGY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLAYAGY+L'TLAYAGY-TLAYD),KEYSAVE                             
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         DROP  R5                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   H4+23(36),TGNAME    AGENCY NAME                                  
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   H4+68(6),TLDVADV    ADVICE CODE                                  
         MVC   H5+11(12),TLDVCID   COMMERCIAL ID                                
*                                                                               
         MVI   BYTE,C'N'           SET BYTE TO INDICATE                         
         LR    R4,R3               IF ADVICE HAS A LIFT                         
         MVI   ELCODE,TALFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   BYTE,C'Y'                                                        
*                                                                               
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   HDHOOKX                                                          
         USING TAFND,R4            VERSION/LIFT                                 
         L     R4,TGELEM                                                        
         MVC   TGVER,TAFNNAME                                                   
         MVC   H5+69(1),TAFNNAME                                                
         CLI   BYTE,C'Y'                                                        
         BE    HDHOOK5A                                                         
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    HDHOOK5A                                                         
         EDIT  (1,TAFNNAME),(3,H5+69),ALIGN=LEFT                                
*                                                                               
HDHOOK5A MVC   AIO,AIO3                                                         
*                                                                               
         L     R4,AIO3                                                          
         GOTO1 RECVAL,DMCB,TLCOCDQ,(X'24',0)                                    
         BNE   HDHOOK7                                                          
*                                                                               
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE 3-CHARACTER          
         BZ    HDHOOK5D            VERSIONS                                     
         CLI   TGVER,26            AND VERSION CODE IS GREATER THAN             
         BNH   HDHOOK5D            26                                           
*                                                                               
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX         FIND RECORD EQUATE FOR THIS                  
HDHOOK5B CLI   0(RE),X'FF'         VERSION NUMBER                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TGVER,VINDUPLM                                                   
         BNH   HDHOOK5C                                                         
         LA    RE,VINDLNQ(RE)                                                   
         B     HDHOOK5B                                                         
*                                                                               
HDHOOK5C XC    KEY,KEY              GET COMMERCIAL RECORD FOR                   
         MVC   KEY(L'TLCOKEY),0(R4) THAT VERSION                                
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   HDHOOK7                                                          
         GOTO1 GETREC                                                           
         DROP  RE                                                               
*                                                                               
HDHOOK5D MVI   ELCODE,TAVRELQ       R4=A(TAVR ELEMENT)                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
HDHOOK6  BAS   RE,NEXTEL                                                        
         BNE   HDHOOK7                                                          
         USING TAVRD,R4                                                         
         CLC   TAVRVERS,TGVER       GET TAVR ELEMENT WITH MATCHING              
         BNE   HDHOOK6              VERSION LETTER                              
         MVC   H5+73(12),TAVRCID    AND MOVE DESCRIPTION TO REPORT              
         DROP  R4                                                               
*                                                                               
HDHOOK7  MVC   AIO,AIO1                                                         
         STC   R2,ELCODE           RESTORE ELCODE                               
HDHOOKX  B     XIT                                                              
         EJECT                                                                  
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              LITERALS FOR PRINTED REPORT                                      
         SPACE 1                                                                
LTCLI    DC    C'Client'                                                        
LTPRD    DC    C'Product'                                                       
LTTITLE  DC    C'Title'                                                         
LTLID    DC    C'Lift ID'                                                       
LTLSEC   DC    C'Lift Len'                                                      
LTCON    DC    C'Contracts'                                                     
LTFDTE   DC    C'Film Date'                                                     
LTRDTE   DC    C'Recd Date'                                                     
LTSTUDIO DC    C'Studio'                                                        
LTCITY   DC    C'City'                                                          
LTCYST   DC    C'City/State'                                                    
LTAUTH   DC    C'Auth/po'                                                       
LTEST    DC    C'Estimate'                                                      
LTESPD   DC    C'Period'                                                        
LTMED    DC    C'Media'                                                         
LTSEC    DC    C'Len'                                                           
LTEXP    DC    C'Exp'                                                           
LTDUE    DC    C'Due'                                                           
LTCMNT   DC    C'Comment'                                                       
LTUSE    DC    C'Use Type'                                                      
LTUSE2   DC    C'Pay Also'                                                      
LT2NDPAY DC    C'<---- There are two (2) payments on this advice!'              
LTCYC    DC    C'Cycle'                                                         
LTINV    DC    C'Assigned to Invoice'                                           
LTABSS   DC    C'Apply Session?'                                                
LTAHLD   DC    C'Apply Holding Fee?'                                            
LTPREV   DC    C'Previous'                                                      
LTCURR   DC    C'Current  '                                                     
LTMAJ    DC    C'Majors'                                                        
LTUNT    DC    C'Units'                                                         
LTSUB    DC    C'Subscrbrs'                                                     
LTMKTS   DC    C'Markets'                                                       
LTAREA   DC    C'Area'                                                          
LTUNLIM  DC    C'UNLIMITED'                                                     
LTCAST   DC    C'S/S Numb  Performer Name             Cat Cam Uni Dbl LX        
               ft Ov1%   Ov2%'                                                  
LTCAST2  DC    C'--------  --------------             --- --- --- --- -X        
               -- ----   ----'                                                  
LTPCMT   DC    C'Cmnt'                                                          
LTCLA    DC    C'Use# Use Date  Program Name    Lft Nwk'                        
LTCLA2   DC    C'---- --------  ------------    --- ---'                        
         SPACE 1                                                                
LTDATE   DC    C'Date'                                                          
LTDATE2  DC    C'--------'                                                      
LTCBLD   DC    C'CNet'                                                          
LTMKTD   DC    C'Mkt '                                                          
LTLCBD   DC    C'CSys'                                                          
LTCBLD2  DC    C'------'                                                        
         SPACE 1                                                                
LTSPLT   DC    C'Estimate Number   Percent'                                     
LTSPLT2  DC    C'---------------   -------'                                     
         SPACE 1                                                                
LTDETAIL DC    C'DETAILS RECORD EXISTS'                                         
         SPACE 1                                                                
LTDEL    DC    C'*DELETED*'                                                     
LTNTSENT DC    C'** THIS ADVICE HAS NOT BEEN SENT TO TP **'                     
LTNTPAID DC    C'** THIS ADVICE HAS NOT BEEN PAID **'                           
         SPACE 1                                                                
NOSNDFLG DC    C'N'                                                             
ADVTYPE  DC    C'N'                                                             
CYCSTRT  DS    XL3                                                              
CYCEND   DS    XL3                                                              
CYC      DS    CL30                                                             
MUSOTH   DS    CL1                                                              
REGOTH   DS    CL1                                                              
AGYSTAT  DS    XL1                                                              
UNITCALC DS    F                                                                
SVUSE    DS    X                                                                
MAXMKTS  EQU   13                  MAXIMUM MARKETS FOR LINE                     
         DS    0A                                                               
ADCREC   DC    AL4(0)                                                           
RELO     DC    AL4(0)                                                           
         EJECT                                                                  
*              SPECS TO COVER PRINTED ADVICE FORM                               
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SPACE 1                                                                
         SPROG 0,10,20                                                          
         SSPEC H1,2,RUN                                                         
         SSPEC H1,62,REPORT                                                     
         SSPEC H2,62,REQUESTOR                                                  
         SSPEC H4,2,C'AGENCY'                                                   
         SSPEC H4,62,C'ADVICE'                                                  
         SSPEC H5,2,C'COMML ID'                                                 
         SSPEC H5,62,C'VER/LFT'                                                 
         SPACE 1                                                                
         SPROG 0,10                                                             
         SSPEC H1,34,C'Talent Advice'                                           
         SSPEC H2,34,13X'BF'                                                    
         SPACE 1                                                                
         SPROG 0,20                                                             
         SSPEC H1,28,C'Talent Advice Completion Report'                         
         SSPEC H2,28,31X'BF'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATE CYCLE END DATE AND PRINTS CYCLE                
*                                                                               
         USING PRNTD,R3                                                         
         USING TAVUD,R4                                                         
CYCDTS   NTR1  BASE=*,LABEL=*                                                   
         OC    CYCSTRT+3(3),CYCSTRT+3     IF HAVE CYCLE END DATE                
         BZ    PRUS4A                     PRINT OUT THE CYCLE                   
         MVC   PRNTLTAG+46(L'LTCYC),LTCYC                                       
         GOTO1 DATCON,DMCB,(X'11',CYCSTRT),(8,PRNTLEFT+44)                      
         B     CYCDTSX                                                          
*                                                                               
PRUS4A   MVC   CYC,SPACES                      IF INTERMEDIATE ADVICE           
         GOTO1 DATCON,DMCB,(1,CYCSTRT),(8,CYC)                                  
         MVC   CYC+8(2),=C'-('    BEGIN BUILDING CALCULATE CYCLE LINE           
*                                                                               
         GOTO1 USEVAL,DMCB,(X'80',(R5)),(R6)                                    
         MVC   BYTE,TGUSWKS             CALL USEVAL TO GET # OF WEEKS           
*                                                                               
         CLC   TGUSNAME(6),=C'DEALER'                IF USE IS DEALER           
         BNE   *+12                                                             
         MVI   TGUSWKS,X'86'             CYCLE LENGTH WILL NOT BE SET           
         MVI   BYTE,6                                 MUST SET MYSELF           
*                                                                               
         CLI   TGUSWKS,0            IF CYCLE LENGTH NOT SET BY USEVAL           
         BNE   PRUS4D                              SET BASED ON MEDIA           
         MVI   BYTE,13                        13 WEEKS FOR TELEVISION           
         MVI   TGUSWKS,13                                                       
         CLI   TAVUMED,C'R'                     8 WEEKS SET FOR RADIO           
         BNE   *+12                                                             
         MVI   BYTE,8                                                           
         MVI   TGUSWKS,8                                                        
*                                                                               
PRUS4D   NI    BYTE,X'3F' TURN OFF WEEK, MONTH, DAY INDICATOR IN BYTE           
*                                                                               
         TM    TGUSWKS,X'80'       IF CYCLE NOT DEFINED AS MONTHS               
         BO    PRUS4DA                                                          
         TM    TGUSWKS,X'40'       OR DAYS                                      
         BO    PRUS4DA                                                          
         CLI   BYTE,13             AND SET AS 13                                
         BNE   PRUS4DA                                                          
         TM    AGYSTAT,TAAYS13W    AND AGENCY DOESN'T USE 13 WK CYCLES          
         BO    PRUS4DA                                                          
         MVI   BYTE,3              SET AS 3 MONTHS                              
         MVI   TGUSWKS,X'83'                                                    
*                                                                               
PRUS4DA  EDIT  BYTE,(3,CYC+10),ALIGN=LEFT                                       
         LR    R1,R0               MOVE LENGTH TO CALCULATE CYCLE LINE          
         LA    R1,CYC+10(R1)          AND POINT R1 AT NEXT EMPTY SPACE          
*                                                                               
         MVI   0(R1),C'D'          CYCLE IS IN DAYS                             
         TM    TGUSWKS,X'40'       IF TGUSWKS HAS X'40 BIT ON                   
         BO    PRUS4BB                                                          
         MVI   0(R1),C'M'          CYCLE IS IN MONTHS                           
         TM    TGUSWKS,X'80'       IF TGUSWKS HAD X'80' BIT ON                  
         BO    PRUS4BB                                                          
         MVI   0(R1),C'W'                                                       
PRUS4BB  MVI   1(R1),C')'          END WITH TRAILING PARENTHESIS                
*                                                                               
PRUS4C   LA    RE,CYC              CALCULATE CHARACTER LENGTH OF                
         LA    RF,0                CYCLE LENGTH                                 
PRUS5C   CLI   0(RE),C' '          STORE IN RF                                  
         BE    PRUS6C                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     PRUS5C                                                           
*                                                                               
PRUS6C   LA    R2,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(X'40',(R2)),((RF),CYC)                               
         USING PERVALD,R2                                                       
         MVC   CYCEND,PVALPEND     RETURN CYCLE END DATE IN CYCEND              
         MVC   PRNTLEFT+33(L'LTCYC),LTCYC     PRINT CYCLE START DATE            
         GOTO1 DATCON,DMCB,(X'11',CYCSTRT),(8,PRNTLEFT+44)                      
CYCDTSX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT INTERNET/NEW MEDIA DETAILS                      
*              ON ENTRY ... R3=A(PRINT LINE)                                    
*                           R4=A(ADVICE USE DETAILS ELEMENT)                    
         SPACE 1                                                                
         USING PRNTD,R3                                                         
         USING TAVUD,R4                                                         
PRNTINMD NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LTIRND           SET UP COLUMN HEADERS                        
         CLI   TAVUUSE,UIRN                                                     
         JE    PINMD05                                                          
         CLI   TAVUUSE,USIR                                                     
         JE    PINMD05                                                          
         CLI   TAVUUSE,UINU                                                     
         JE    PINMD05                                                          
         CLI   TAVUUSE,USIU                                                     
         JE    PINMD05                                                          
         CLI   TAVUUSE,UMVI                                                     
         JE    PINMD05                                                          
         CLI   TAVUUSE,USMI                                                     
         JE    PINMD05                                                          
         LA    R3,LTNMRD                                                        
         SPACE 1                                                                
PINMD05  L     R4,AIO                                                           
         MVI   ELCODE,TAMDELQ                                                   
         BRAS  RE,GETEL            GET INTERNET/NEWMEDIA ELEMENTS               
         JNE   XIT                                                              
         SPACE 1                                                                
         MVC   P+1(L'LTIRND),0(R3) DISPLAY INTERNET/NEWMEDIA HEADING            
         MVC   P+16(L'LTIRND),0(R3)                                             
         MVC   P+31(L'LTIRND),0(R3)                                             
         MVC   P+46(L'LTIRND),0(R3)                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         LA    R3,P                R3=A(PRINT LINE)                             
         LA    R2,4                SET FOR 4-UP                                 
         J     PINMD20                                                          
         SPACE 1                                                                
PINMD10  BRAS  RE,NEXTEL                                                        
         JNE   PINMD30                                                          
         SPACE 1                                                                
         USING TAMDD,R4                                                         
PINMD20  MVC   PINMCODE,TAMDCODE                                                
         SPACE 1                                                                
         LA    R3,PINMDRHS         POINT TO RHS                                 
         BCT   R2,PINMD10          AND CONTINUE IF OK                           
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)     ALREADY ON RHS SO PRINT LINE                 
         LA    R3,P                AND RESET TO LHS                             
         LA    R2,4                RE-SET FOR 4-UP                              
         J     PINMD10             CONTINUE                                     
         SPACE 1                                                                
PINMD30  CLC   P,SPACES            IF ANYTHING LEFT TO PRINT                    
         JE    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
         J     XIT                                                              
         SPACE 2                                                                
LTIRND   DC    C'Internet '                                                     
LTNMRD   DC    C'New Media'                                                     
         EJECT                                                                  
*              ROUTINE TO PRINT ITN DETAILS                                     
         SPACE 1                                                                
         USING PRNTD,R3                                                         
         USING TAVUD,R4            R4=A(ADVICE USE DETAILS EL.)                 
PRNTITN  NTR1  BASE=*,LABEL=*                                                   
         OC    TAVUUSES,TAVUUSES       IF USES NOT PRESENT                      
         BNZ   *+14                                                             
         MVC   PRNTLEFT+12(4),=C'AUTO' MUST BE AUTO PAYMENT                     
         B     PITNX                                                            
         SPACE 1                                                                
         LA    R3,PRNTLEFT+12                                                   
         LH    RE,TAVUUSES                                                      
         LH    RF,TAVUUNT                                                       
         SR    RF,RE                                                            
         AHI   RF,1                                                             
         EDIT  (RF),(4,(R3)),ALIGN=LEFT                                         
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),C'-'                                                       
         EDIT  TAVUUNT,(3,1(R3)),ZERO=NOBLANK,ALIGN=LEFT                        
PITNX    XIT1                                                                   
         DROP  R3,R4                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CALCULATE CABLE UNITS                                 
CCBLUNIT NTR1  BASE=*,LABEL=*                                                   
         XC    UNITCALC,UNITCALC                                                
         XC    SUBCOUNT,SUBCOUNT                                                
         XC    FULL,FULL                                                        
         XC    TGDUB,TGDUB                                                      
         SPACE 1                                                                
         USING TAUPD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BE    CCUNIT05                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAUPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   CCUNIT05                                                         
         MVC   UNITCALC+2(L'TAUPICBU),TAUPICBU                                  
         CLI   SVUSE,ULCB                                                       
         BNE   CCUNIT05                                                         
         MVC   UNITCALC,TAUPISUB                                                
         DROP  R4                                                               
         SPACE 1                                                                
CCUNIT05 MVC   AIO,AIO2                                                         
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   BYTE,TAVUUSE                                                     
         MVI   TGMTTYPE,TANPNET    READ CNET RECORDS                            
         CLI   TAVUUSE,UCBL        IF USE IS CABLE                              
         BE    CCUNIT10                                                         
         CLI   TAVUUSE,USCB        OR SPANISH CABLE                             
         BE    CCUNIT10                                                         
         MVI   TGMTTYPE,C'S'       READ CSYS RECORDS                            
         CLI   TAVUUSE,ULCB        IF USE IS LOCAL CABLE                        
         BE    CCUNIT10                                                         
         MVI   TGMTTYPE,C'T'       READ TMKT RECORDS                            
         CLI   TAVUMED,C'R'        IF MEDIA IS NOT RADIO                        
         BNE   CCUNIT10                                                         
         MVI   TGMTTYPE,C'R'       ELSE, READ RMKT RECORDS                      
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMTD,R4                                                         
CCUNIT10 L     R4,AIO1           READ THROUGH CABLE STATION RECORDS             
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    CCUNIT25                                                         
         XC    UNITCALC,UNITCALC                                                
         XC    SUBCOUNT,SUBCOUNT                                                
         B     CCUNIT70                                                         
CCUNIT20 BRAS  RE,NEXTEL                                                        
         BNE   CCUNIT70                                                         
CCUNIT25 C     R4,FULL           IF CABLE STATION ALREADY PROCESSED             
         BNH   CCUNIT20          GET NEXT CABLE STATION ELEMENT                 
         ST    R4,FULL                                                          
         CLC   TAMTCODE,TGDUB                                                   
         BE    CCUNIT20                                                         
         MVC   TGDUB(L'TAMTCODE),TAMTCODE                                       
         LHI   RF,TLMTCDQ                                                       
         BRAS  RE,USEALPH                                                       
         BNE   *+8                                                              
         LHI   RF,TLMTALDQ                                                      
         GOTO1 RECVAL,DMCB,(RF),(X'A4',TAMTCODE)                                
         BE    CCUNIT30                                                         
         CLI   TGMTTYPE,TANPNET                                                 
         BNE   CCUNIT20                                                         
         MVI   TGMTTYPE,C'S'                                                    
         GOTO1 RECVAL,DMCB,TLMTCDQ,(X'A4',TAMTCODE)                             
         MVI   TGMTTYPE,TANPNET                                                 
         BNE   CCUNIT20                                                         
         B     CCUNIT65                                                         
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMSD,R4                                                         
CCUNIT30 L     R4,AIO2           GET WEIGHT FOR THE PROPER YEAR                 
         MVI   ELCODE,TAMSELQ    FROM NET CABLE RECORD                          
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
CCUNIT50 BRAS  RE,NEXTEL                                                        
         BE    CCUNIT55                                                         
         CLI   BYTE,USWS                                                        
         BNE   CCUNIT10                                                         
         L     RE,UNITCALC                                                      
         AHI   RE,1                                                             
         ST    RE,UNITCALC                                                      
         B     CCUNIT10                                                         
CCUNIT55 CLI   BYTE,USWS                                                        
         BNE   CCUNIT60                                                         
         CLI   TAMSTYPE,C'S'                                                    
         BNE   CCUNIT50                                                         
CCUNIT60 L     RE,UNITCALC                                                      
         L     RF,TAMSWGHT                                                      
         AR    RE,RF                                                            
         ST    RE,UNITCALC                                                      
         B     CCUNIT10                                                         
         DROP  R4                                                               
         SPACE 1                                                                
CCUNIT65 L     RF,UNITCALC                                                      
         AHI   RF,1                                                             
         ST    RF,UNITCALC                                                      
         SPACE 1                                                                
         USING TAMSD,R4                                                         
         L     R4,AIO2           GET WEIGHT FOR THE PROPER YEAR                 
         MVI   ELCODE,TAMSELQ    FROM NET CABLE RECORD                          
         BRAS  RE,GETEL                                                         
         BNE   CCUNIT10                                                         
         L     RF,SUBCOUNT                                                      
         A     RF,TAMSWGHT                                                      
         ST    RF,SUBCOUNT                                                      
         B     CCUNIT10                                                         
         DROP  R4                                                               
         SPACE 1                                                                
CCUNIT70 OC    SUBCOUNT,SUBCOUNT                                                
         BZ    CCUNITX                                                          
         XR    RE,RE                                                            
         L     RF,SUBCOUNT                                                      
         D     RE,=F'350000'                                                    
         A     RF,UNITCALC                                                      
         ST    RF,UNITCALC                                                      
         SPACE 1                                                                
CCUNITX  MVC   AIO,AIO1                                                         
         XIT1                                                                   
         SPACE                                                                  
SUBCOUNT DS    F                                                                
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DETERMINE IF ALPHA CODE SHOULD BE USED TO             
*              READ FOR TMKT RECORDS                                            
         SPACE 1                                                                
USEALPH  NTR1  BASE=*,LABEL=*                                                   
         USING TAVUD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   UANO                                                             
         CLI   TAVUMED,C'T'                                                     
         BNE   UANO                                                             
         CLI   TAVUUSE,UCBL                                                     
         BE    UANO                                                             
         CLI   TAVUUSE,USCB                                                     
         BE    UANO                                                             
         CLI   TAVUUSE,ULCB                                                     
         BE    UANO                                                             
         DROP  R4                                                               
UAYES    XR    RC,RC                                                            
UANO     LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  R7                  DROP SECONDARY BASE REGISTERS                
       ++INCLUDE TADCREC                                                        
         EJECT                                                                  
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
*              DSECT TO ADVICE PRINT LINE                                       
         SPACE 1                                                                
PRNTD    DSECT                     UPPER PORTION OF FORM                        
         DS    CL1                                                              
PRNTLTAG EQU   *                                                                
         DS    CL10                                                             
PRNTLEFT EQU   *                                                                
PRNTSDTE EQU   *                                                                
PRNTAUTH DS    0CL16                                                            
         DS    CL13                                                             
PRNTLFLT EQU   *                                                                
         DS    CL4                                                              
PRNTCTAG EQU   *                                                                
         DS    CL5                                                              
PRNTLSEC EQU   *                                                                
         DS    CL1                                                              
PRNTCTR  EQU   *                                                                
         DS    CL4                                                              
PRNTSSTU EQU   *                                                                
PRNTEST  DS    0CL16                                                            
         DS    CL17                                                             
PRNTRTAG EQU   *                                                                
         DS    CL7                                                              
PRNTRHS  EQU   *                                                                
         DS    CL5                                                              
PRNTRTG2 EQU   *                                                                
         DS    CL4                                                              
PRNTRHS2 EQU   *                                                                
         SPACE 1                                                                
         ORG   PRNTD               DSECT TO COVER PERFORMER DETAILS             
         DS    CL1                                                              
PRNTACTL DS    CL1                 '*' IF ACTUAL CAST MEMBER                    
PRNTSSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
         DS    CL1                                                              
PRNTNAME DS    CL26                PERFORMER NAME                               
         DS    CL1                                                              
PRNTCAT  DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
PRNTCAM  DS    CL3                 ON/OFF CAMERA                                
         DS    CL1                                                              
PRNTUNI  DS    CL3                 UNION                                        
         DS    CL2                                                              
PRNTDBL  DS    CL1                 DOUBLES                                      
         DS    CL3                                                              
PRNTLFT  DS    CL1                 LIFT STATUS                                  
         DS    CL2                                                              
PRNTOV1  DS    CL6                 OVERSCALE RATE 1                             
         DS    CL1                                                              
PRNTOV2  DS    CL6                 OVERSCALE RATE 2                             
         ORG   PRNTSSN+132                                                      
PRNTTDEL DS    CL9                 '*DELETED*' TAG                              
         ORG   PRNTNAME+132                                                     
PRNTCMTT DS    CL4                 COMMENT TAG                                  
         DS    CL1                                                              
PRNTCMT  DS    CL60                COMMENT                                      
         SPACE 1                                                                
         ORG   PRNTD               DSECT TO COVER CLA DETAILS                   
         DS    CL1                                                              
PCLAUSEN DS    CL4                 USE NUMBER                                   
         DS    CL1                                                              
PCLADATE DS    CL8                 USE DATE                                     
         DS    CL2                                                              
PCLAPNME DS    CL15                PROGRAM NAME                                 
         DS    CL2                                                              
PCLALFT  DS    CL1                 LIFT                                         
         DS    CL3                                                              
PCLANWK  DS    CL1                 NETWORK                                      
         DS    CL2                                                              
PCLARHS  EQU   *                                                                
         ORG   PRNTD               DSECT TO COVER CBL DETAILS                   
         DS    CL1                                                              
PCBLDATE DS    CL8                 FIRST USE DATE                               
         DS    CL1                                                              
PCBLCODE DS    CL6                 CNET/MKT/CSYS CODE                           
         DS    CL5                                                              
PCBLDRHS EQU   *                                                                
         ORG   PRNTD               DSECT TO COVER INTERNET/NEWMEDIA             
         DS    CL1                                                              
PINMCODE DS    CL4                 INTERNET/NEWMEDIA CODE                       
         DS    CL10                                                             
PINMDRHS EQU   *                                                                
         SPACE 1                                                                
         ORG   PRNTD               DSECT TASID ELEMENTS(ASPLIT INFO)            
         DS    CL1                                                              
PSPLTEST DS    CL16                ESTIMATE NUMBER                              
         DS    CL2                                                              
PSPLTPCT DS    CL7                 PERCENTAGE                                   
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024TAGEN8E   10/04/16'                                      
         END                                                                    
