*          DATA SET SPREPSU02  AT LEVEL 015 AS OF 05/07/13                      
*PHASE SPSU02A                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'SPSU02  SUPERDESK OVERNIGHT UPDATE'                             
SPSU02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPSU02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPSU02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         CLI   MODE,REQFRST                                                     
         BE    INIT                                                             
         B     EXIT                                                             
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
*                                                                               
INIT     DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,0,X'D9000AB7'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ASPAUTH,0(R1)            GET SPAUTH ADDRESS                      
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         GOTO1 DATCON,DMCB,(3,TODAYB),(19,FULL)  GET TODAY'S DATE               
         L     R1,=A(SAUTHTAB)                                                  
         ST    R1,ASAUTHTB                                                      
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SU10NX   GET   RECVIN,RCVREC-4                                                  
         LA    RE,RCVREC-4                                                      
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)                                                    
*                                                                               
         CLI   RRPRG,X'01'         SKIP PFM CHANGES !!!!                        
         BE    SU10NX                                                           
*                                                                               
         LA    R6,RKEY                                                          
         CLI   RRFILTY,X'21'       SPTFIL?                                      
         BNE   SU12                                                             
         CLC   0(2,R6),=X'0D34'    CHECK IF DARE RECORD                         
         BE    SU20                                                             
         B     SU10NX              SKIP TO NEXT                                 
*                                                                               
SU12     CLI   RRFILTY,X'37'       XSPFIL?                                      
         BNE   SU10NX                                                           
         CLC   0(2,R6),=X'0D39'    CHECK IF AUTH RECORD                         
         BE    SU80                                                             
         B     SU10NX              SKIP TO NEXT                                 
*===============================================================*               
* PUT DARE REC DETAILS TO SORTER WITH TYPE D                                    
*===============================================================*               
         USING DAREORDD,R6                                                      
SU20     CLI   RRRECTY,2           MUST BE A CHANGE OR SKIP                     
         BNE   SU10NX                                                           
         MVC   SVAGMED,DOKAGMD     SAVE AGENCY/MEDIA                            
         MVC   SVQAGY,DORAGY                                                    
*                                                                               
         XC    SVSENTDT,SVSENTDT                                                
         XC    SVCONFDT,SVCONFDT                                                
*                                                                               
         LA    R6,DORFRST                                                       
SU25     CLI   0(R6),X'00'                                                      
         BE    SU50                                                             
         CLI   0(R6),X'12'                                                      
         BE    SU30                                                             
SU25NX   ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     SU25                                                             
*                                                                               
         USING DOSTELD,R6                                                       
SU30     CLI   DOSTSTAT,DSENT                                                   
         BNE   SU33                                                             
         OC    SVSENTDT,SVSENTDT                                                
         BNZ   SU25NX              SKIP IF HAVE LATER DATE                      
         GOTO1 DATCON,DMCB,(8,DOSTDATE),(2,SVSENTDT)                            
         B     SU25NX                                                           
*                                                                               
SU33     CLI   DOSTSTAT,QCFMD                                                   
         BNE   SU25NX                                                           
         OC    SVCONFDT,SVCONFDT                                                
         BNZ   SU25NX              SKIP IF HAVE LATER DATE                      
         GOTO1 DATCON,DMCB,(8,DOSTDATE),(2,SVCONFDT)                            
         B     SU25NX                                                           
*                                                                               
SU50     OC    SVCONFDT,SVCONFDT   DID WE CONFIRM?                              
         BZ    SU10NX              NO THEN SKIP                                 
*                                                                               
         USING DAREORDD,R6                                                      
SU55     LA    R6,RKEY                                                          
         LA    R6,DORFRST                                                       
         CLI   0(R6),X'01'                                                      
         BNE   SU10NX                                                           
         USING DOIDELD,R6                                                       
*                                                                               
         LA    R2,SORTREC                                                       
         XC    SORTREC,SORTREC                                                  
         USING SRTRECD,R2                                                       
         MVI   SRTTYPE,SRTTDAR     D FOR DARE                                   
         MVC   SRTAGMD,SVAGMED     AGENCY/MEDIA                                 
         MVC   SRTCLT,DOIDCLT      CLIENT                                       
         MVC   SRTPRD,DOIDPRD      PRODUCT                                      
         MVC   SRTPRD2,DOIDPRD2    PRODUCT2                                     
         MVC   SRTEST,DOIDEST      ESTIMATE                                     
         MVC   SRTFLT,DOIDFLTN     FLIGHT NUMBER                                
         MVC   SRTSTA,DOISTA       STATION                                      
         MVC   SRTQAGY,SVQAGY      ALPHA AGY                                    
         MVC   SRTSENTD,SVSENTDT   ORDER SENT DATE                              
         MVC   SRTCONFD,SVCONFDT   ORDER CONF DATE                              
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
         CLI   QOPT5,C'Y'          TRACE                                        
         BNE   SU10NX                                                           
         GOTO1 CLUNPK,DMCB,SRTCLT,PCLIENT                                       
         GOTO1 HEXOUT,DMCB,SRTPRD,PPRD,1,=C'TOG'                                
         GOTO1 HEXOUT,DMCB,SRTFLT,PPRD2,1,=C'TOG'                               
**       MVC   PPRD,SRTPRD                                                      
**       MVC   PPRD2,SRTPRD2                                                    
         EDIT  (B1,SRTEST),PEST,FILL=0                                          
         MVC   WORK(2),=X'0000'                                                 
         MVC   WORK+2(3),SRTSTA                                                 
         GOTO1 MSUNPK,DMCB,WORK,PMKT,PSTAT                                      
*                                                                               
         OC    SRTSENTD,SRTSENTD                                                
         BZ    SU60                                                             
         GOTO1 DATCON,DMCB,(2,SRTSENTD),(5,PSENTDT)                             
SU60     OC    SRTCONFD,SRTCONFD                                                
         BZ    SU62                                                             
         GOTO1 DATCON,DMCB,(2,SRTCONFD),(5,PCONFDT)                             
         MVC   P+90(7),=C'SORTPUT'                                              
         MVC   P+98(20),SORTREC                                                 
SU62     GOTO1 REPORT                                                           
*                                                                               
         B     SU10NX                                                           
         DROP  R2,R6                                                            
*===============================================================*               
* PUT AUTH REC DETAILS TO SORTER WITH TYPE A                                    
*===============================================================*               
         USING AUTRECD,R6                                                       
*                                                                               
SU80     OC    AUTKMKT,AUTKMKT     ONLY USE HIGH LEVEL AUTHS                    
         BNZ   SU10NX                                                           
*                                                                               
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC                                                       
         XC    SORTREC,SORTREC                                                  
         MVI   SRTTYPE,SRTTAUT     A FOR AUTHS                                  
         MVC   SRTAGMD,AUTKAM      AGENCY/MEDIA                                 
         MVC   SRTCLT,AUTKCLT      CLIENT                                       
         MVC   SRTPRD,AUTKPRD      PRODUCT                                      
         MVC   SRTPRD2,AUTKPRD2    PRODUCT2                                     
         MVC   SRTEST,AUTKEST      ESTIMATE                                     
         MVC   SRTQAGY,AUTRAGYA    AGY ALPHA                                    
         DROP  R2,R6                                                            
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
         CLI   QOPT5,C'Y'          TRACE                                        
         BNE   SU10NX                                                           
         MVC   P(7),=C'SORTPUT'                                                 
         MVC   P+10(20),SORTREC                                                 
         GOTO1 REPORT                                                           
         B     SU10NX                                                           
*===============================================================*               
* END OF REQUEST FILE - NOW GET SORT RECS                                       
*===============================================================*               
SU99     CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SAVESRT,SAVESRT                                                  
         B     *+10                DON'T SAVE FIRST KEY                         
SU200GT  MVC   SAVESRT(L'SORTREC),0(R2)                                         
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)                                                      
         BZ    SU202                                                            
*                                                                               
*        MOVE FROM R5 TO SORTREC                                                
         LA    R2,SORTREC                                                       
         USING SRTRECD,R2                                                       
         MVC   SORTREC,0(R5)                                                    
         CLC   SORTREC,SAVESRT                  IS ENTIRE KEY THE SAME?         
         BE    SU200GT                          IF YES, SKIP                    
         MVI   DMINBTS,0           SKIP DELETES                                 
         MVI   DMOUTBTS,0                                                       
         B     SU205                                                            
*                                                                               
SU202    GOTO1 =V(SORTER),DMCB,=C'END'                                          
         GOTO1 AENDREQ                                                          
*                                                                               
*===============================================================*               
* GET CORRECT CLIENT RECORD FOR EITHER TYPE - SAVE CPROF                        
*===============================================================*               
SU205    CLC   SRTAGMD(SRTPRD-SRTAGMD),SAVESRT+1    SAME A/M CLT?               
         BE    SU210               YES, THEN ALREADY HAVE CLT REC               
*                                                                               
         XC    KEY,KEY             READ CLIENT RECORD                           
         LA    R4,KEY                                                           
         USING CLTHDR,R4                                                        
         MVC   CKEYAM,SRTAGMD      AGENCY/MEDIA                                 
         MVC   CKEYCLT,SRTCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETCLT                                                           
         L     R4,ADCLT                                                         
         MVC   SVCPROF,CPROF       SAVE CPROF FOR LATER                         
         DROP  R4                                                               
*                                                                               
SU210    MVC   BYTE,SRTPRD                                                      
         BAS   RE,CONVPRD          CONVERT PRODUCT CODE TO MNEMONIC             
         MVC   QPRD,THREE                                                       
         OC    SRTPRD2,SRTPRD2                                                  
         BZ    SU220                                                            
         MVC   BYTE,SRTPRD2                                                     
         BAS   RE,CONVPRD                                                       
         MVC   QPRD2,THREE                                                      
*===============================================================*               
* SPLIT TYPES - DARE OR AUTH                                                    
*===============================================================*               
SU220    CLI   SRTTYPE,SRTTDAR     DARE REC?                                    
         BE    SU250                                                            
         CLI   SRTTYPE,SRTTAUT     AUTH REC?                                    
         BE    SU300                                                            
         B     SU200GT             GET NEXT REC                                 
*===============================================================*               
* PROCESS DARE UPDATING                                                         
*===============================================================*               
SU250    CLC   SRTAGMD(SRTFLT-SRTAGMD),SAVESRT+1 SAME THRU EST?                 
         BNE   SU252               NO GET EST                                   
         CLC   SRTTYPE(SRTAGMD-SRTTYPE),SAVESRT  SAME TYPE?                     
         BE    SU255                                                            
SU252    BAS   RE,READEST          READ ESTIMATE RECORD                         
         BNE   SU200GT             IF NO AUTHORIZATION OPEN SKIP                
*                                                                               
         USING ESTHDR,R4                                                        
SU255    L     R4,ADEST            DOUBLE CHECK EST IS SD EST                   
         TM    EFLAG1,EF1SDE                                                    
         BNO   SU200GT             IF NO AUTHORIZATION OPEN SKIP                
         DROP  R4                                                               
*                                                                               
         CLI   SRTFLT,0            IF FLIGHT NUMBER=0 USE EST START             
         BE    SU260                                                            
         BAS   RE,READFLT          READ FLIGHT RECORD                           
         BNE   SU200GT             IF NO FLIGHT RECORD, SKIP                    
*                                                                               
SU260    CLC   SRTAGMD(SRTPRD-SRTAGMD),SAVESRT+1    SAME A/M CLT?               
         BNE   SU265                                                            
         CLC   SRTTYPE(SRTAGMD-SRTTYPE),SAVESRT  SAME TYPE?                     
         BNE   SU265                                                            
         CLC   SRTSTA,SAVESRT+8    IS STATION THE SAME?                         
         BE    SU270                                                            
SU265    BAS   RE,GETMKT           GET MARKET FROM STATION RECORD               
*                                                                               
SU270    DS    0H                                                               
*   CALL SPAUTH AS IN SPDAR09                                                   
         USING SPAUTHD,R3                                                       
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   SPACOM,ACOMFACS                                                  
         LA    RF,IOAREA                                                        
         ST    RF,SPAIO                                                         
*                                                                               
         MVI   SPAUPDT,SPAUPDCN     DOING DARE CONFIRMATIONS                    
         MVC   SPAKAM,SRTAGMD                                                   
         MVC   SPAKCLT,SRTCLT                                                   
         MVC   SPAKPRD,SRTPRD                                                   
         MVC   SPAKPRD2,SRTPRD2                                                 
         MVC   SPAKEST,SRTEST                                                   
*                                                                               
         CLI   SPAKPRD2,0          ANY PIGGYBACKS?                              
         BE    SU275                                                            
         CLC   QPRD,QPRD2          IF PRODUCTS NOT IN ALPHABETICAL              
         BL    SU275               ORDER, SWAP THEM                             
         XC    SPAKPRD2,SPAKPRD                                                 
         XC    SPAKPRD,SPAKPRD2                                                 
         XC    SPAKPRD2,SPAKPRD                                                 
*                                                                               
SU275    GOTO1 DATCON,DMCB,(3,SVFLSTD),(2,SPASDTE)     DATE                     
         GOTO1 DATCON,DMCB,(3,SVFLEND),(2,SPAEDTE)     DATE                     
*                                                                               
         MVC   SPAKMKT,BMKT                                                     
         MVC   SPAKSTA,SRTSTA                                                   
         MVC   SPADRSDT,SRTSENTD   DARE SENT DATE                               
         MVC   SPADRCNF,SRTCONFD   DARE CONF DATE                               
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BNE   *+8                                                              
         OI    SPAFLAG,SPAFNOWR    TELL SPAUTH WRITE=NO                         
*                                                                               
         GOTO1 ASPAUTH,ELEM                                                     
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT5,C'Y'          TRACE                                        
         BNE   SU280                                                            
         MVC   P(8),=C'DAREAUTH'                                                
         MVC   P+10(50),0(R3)                                                   
         GOTO1 REPORT                                                           
*                                                                               
SU280    DS    0H                  REAL REPORT - CONFIRMATIONS                  
         TM    SPUPSTAT,SPUPSTCG   CONFIRMED DATE UPDATED BY SPAUTH?            
         BNO   SU299               SKIP REPORT IF NOT                           
*                                                                               
         GOTO1 CLUNPK,DMCB,SRTCLT,PCLIENT                                       
**       MVC   PPRD,SRTPRD                                                      
**       MVC   PPRD2,SRTPRD2                                                    
         GOTO1 HEXOUT,DMCB,SRTPRD,PPRD,1,=C'TOG'                                
         EDIT  (B1,SRTEST),PEST,FILL=0                                          
         GOTO1 MSUNPK,DMCB,SPAKMKT,PMKT,PSTAT                                   
         GOTO1 DATCON,DMCB,(2,SPASDTE),(5,PSTFLT)                               
         GOTO1 DATCON,DMCB,(2,SPAEDTE),(5,PENDFLT)                              
*                                                                               
         OC    SPADRSDT,SPADRSDT                                                
         BZ    SU282                                                            
         GOTO1 DATCON,DMCB,(2,SPADRSDT),(5,PSENTDT)                             
SU282    OC    SPADRCNF,SPADRCNF                                                
         BZ    SU284                                                            
         GOTO1 DATCON,DMCB,(2,SPADRCNF),(5,PCONFDT)                             
*                                                                               
SU284    GOTO1 REPORT                                                           
*                                                                               
SU299    B     SU200GT             GET NEXT RECORD                              
         DROP  R3                                                               
         EJECT                                                                  
*===============================================================*               
* PROCESS AUTH UPDATING                                                         
*===============================================================*               
SU300    DS    0H                                                               
         XC    LASTMSTA,LASTMSTA   START FRESH                                  
         USING BUYREC,R6                                                        
         LA    R6,KEY                                                           
         XC    KEY,KEY             START READING THROUGH BUYS                   
         MVC   BUYKAM,SRTAGMD                                                   
         MVC   BUYKCLT,SRTCLT                                                   
         MVC   BUYKPRD,SRTPRD                                                   
         GOTO1 HIGH                                                             
         B     SU310                                                            
SU310SEQ GOTO1 SEQ                                                              
SU310    CLC   KEY(4),KEYSAVE      SAME A/M,CLT,PRD                             
         BE    SU312               YES - KEEP CHECKING                          
         OC    LASTMSTA,LASTMSTA   IS THERE A LAST STATION TO PROC              
         BZ    *+8                                                              
         BAS   RE,PROCTAB          NO - PROCESS LAST TABLE                      
         B     SU200GT             GET NEXT SORT REC                            
*                                                                               
SU312    LA    R6,KEY                                                           
         TM    KEY+13,X'80'        TEST DELETED                                 
         BO    SU310SEQ                                                         
         CLC   BUYKEST,SRTEST      IS IT THE RIGHT EST?                         
         BNE   SU310SEQ                                                         
*                                                                               
         CLI   KEY+10,0            IF KEY+10 NOT = 0 OR FF                      
         BE    SU314               THEN SKIP IT IT'S SPILL                      
         CLI   KEY+10,X'FF'                                                     
         BNE   SU310SEQ                                                         
*                                                                               
SU314    MVI   DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         TM    15(R6),X'80'        BUY RECORD DELETED?                          
         BO    SU310SEQ                                                         
         LA    R6,KEY              POINT BACK TO KEY                            
*                                                                               
         CLI   SRTPRD2,0           CHECK FOR PIGGY                              
         BE    SU318                                                            
*  CHECK PIGGY?     <----                                                       
*                                                                               
SU318    CLC   BUYKMSTA,LASTMSTA   SAME AS LAST MKT/STA                         
         BE    SU320               YES - PROCESS BUY                            
*                                                                               
         OC    LASTMSTA,LASTMSTA   IS THERE A LAST STATION TO PROC              
         BZ    *+8                                                              
         BAS   RE,PROCTAB          NO - PROCESS LAST TABLE                      
*                                  THEN GET NEW                                 
         BAS   RE,MISSAUTH         TABLE OF MISSING AUTHS FOR MKT/STA           
         MVC   LASTMSTA,BUYKMSTA                                                
*                                                                               
SU320    MVC   SVMKTSTA,BUYKMSTA   SAVE BUY MARKET/STATION                      
         BAS   RE,GETNWS           GET NWS XFR DATE AND WORK REC ADD            
*                                                                               
SU330    L     R6,ADBUY            CHECK ELEMS WITH TABLE                       
         LA    R6,BDELEM                                                        
SU333    CLI   0(R6),0                                                          
         BE    SU310SEQ            CHECK NEXT BUY                               
         CLI   0(R6),6                                                          
         BL    SU334                                                            
         CLI   0(R6),13                                                         
         BNH   SU335                                                            
SU334    ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     SU333                                                            
*                                                                               
         USING SAUTHD,RE                                                        
SU335    L     RE,ASAUTHTB         POINT TO TABLE                               
         CLC   0(2,RE),=X'FFFF'    EOT                                          
         BE    SU310SEQ            NO TABLE = ALL STN AUTHS EXIST               
*                                                                               
SU340    CLC   2(2,R6),0(RE)       SPOT TO AUTH START                           
         BL    SU345                                                            
         CLC   2(2,R6),2(RE)       SPOT TO AUTH END                             
         BNH   SU350               NEED TO ADD AUTH                             
*                                                                               
SU345    LA    RE,SALENQ(RE)       NEXT                                         
         CLC   0(2,RE),=X'FFFF'    EOT                                          
         BE    SU334               NEXT ELEM                                    
         B     SU340                                                            
*                                                                               
SU350    MVI   SASPOTS,C'Y'        INDICATE SPOTS FOUND                         
         OC    NWSXFRDT,NWSXFRDT                                                
         BZ    SU355                                                            
         CLC   SANWSDT,NWSXFRDT                                                 
         BH    *+10                                                             
         MVC   SANWSDT,NWSXFRDT    USE LATEST NWS XFR DATE                      
*                                                                               
SU355    OC    WRKADDDT,WRKADDDT   ANY WORK ADDED DATE                          
         BZ    SU334               NEXT ELEM                                    
         OC    SAWRKDT,SAWRKDT     ANY WORK DATE IN TABLE                       
         BZ    *+14                NO THEN PUT IT IN                            
         CLC   WRKADDDT,SAWRKDT    IF THERE IS ONE                              
         BH    *+10                                                             
         MVC   SAWRKDT,WRKADDDT    USE EARLIEST WORK DATE                       
         B     SU334               NEXT ELEM                                    
*                                                                               
         DROP  RE                                                               
         EJECT                                                                  
*===============================================================*               
* CALL SPAUTH TO BUILD TABLE OF MISSING AUTHS                                   
*===============================================================*               
*                                                                               
MISSAUTH NTR1                                                                   
         USING SPAUTHD,R3                                                       
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   SPACOM,ACOMFACS                                                  
         LA    RF,IOAREA                                                        
         ST    RF,SPAIO                                                         
*                                                                               
         MVC   SPAKAM,SRTAGMD                                                   
         MVC   SPAKCLT,SRTCLT                                                   
         MVC   SPAKPRD,SRTPRD                                                   
         MVC   SPAKPRD2,SRTPRD2                                                 
         MVC   SPAKEST,SRTEST                                                   
         MVC   SPAKMKT(L'BUYKMSTA),BUYKMSTA                                     
*                                                                               
         L     R1,=A(AUTHTAB)      BUILD TABLE OF PERIODS                       
         ST    R1,SPAFATBL         THAT -DON'T- HAVE STN AUTHS                  
         OI    SPAFLAG,SPAFTBL                                                  
*                                                                               
         CLI   SPAKPRD2,0          ANY PIGGYBACKS?                              
         BE    MISS10                                                           
         CLC   QPRD,QPRD2          IF PRODUCTS NOT IN ALPHABETICAL              
         BL    MISS10              ORDER, SWAP THEM                             
         XC    SPAKPRD2,SPAKPRD                                                 
         XC    SPAKPRD,SPAKPRD2                                                 
         XC    SPAKPRD2,SPAKPRD                                                 
*                                                                               
MISS10   CLI   RCWRITE,C'N'                                                     
         BNE   *+8                                                              
         OI    SPAFLAG,SPAFNOWR    TELL SPAUTH WRITE=NO                         
*                                                                               
         GOTO1 ASPAUTH,ELEM                                                     
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT5,C'Y'          TRACE                                        
         BNE   MISS12                                                           
         MVC   P(7),=C'MISSTAB'                                                 
         MVC   P+10(20),ELEM                                                    
         GOTO1 REPORT                                                           
*                                                                               
*  BUILD NEW MISSING TABLE TO LEAVE SPACE TO INCLUDE NWS DATES                  
*                                                                               
         USING SAUTHD,R4                                                        
MISS12   L     R4,ASAUTHTB         POINT TO NEW STATION TABLE                   
*                                                                               
         L     R1,SPAFATBL         AND TABLE FROM SPAUTH                        
MISS20   CLC   0(2,R1),=X'FFFF'    ANY MISSING AUTHS                            
         BE    MISSX                                                            
*                                                                               
         XC    0(SALENQ,R4),0(R4)  CLEAR ENTRY                                  
         MVC   SASTDT,0(R1)        COPY DATES                                   
         MVC   SAENDT,2(R1)                                                     
*                                                                               
         AHI   R1,4                                                             
         LA    R4,SALENQ(R4)       NEXT                                         
         B     MISS20                                                           
*                                                                               
MISSX    MVC   0(2,R4),=X'FFFF'    SET EOT                                      
         B     EXIT                                                             
         DROP  R3                                                               
*===============================================================*               
* PROCESS MISSING AUTH TABLE                                                    
*===============================================================*               
*                                                                               
PROCTAB  NTR1                                                                   
         CLI   QOPT5,C'Y'          TRACE                                        
         BNE   PT00                                                             
         MVC   P(7),=C'PROCTAB'                                                 
         L     R1,ASAUTHTB                                                      
         MVC   P+10(120),0(R1)                                                  
         GOTO1 REPORT                                                           
*                                                                               
         USING SAUTHD,R4                                                        
PT00     L     R4,ASAUTHTB         POINT TO TABLE                               
*                                                                               
PT10     CLC   0(2,R4),=X'FFFF'    EOT                                          
         BE    PTX                 DONE                                         
         CLI   SASPOTS,C'Y'        ANY SPOTS FOR THIS FLIGHT?                   
         BE    PT30                YES THEN CALL SPAUTH                         
PT20     LA    R4,SALENQ(R4)       NEXT                                         
         B     PT10                                                             
*                                                                               
PT30     BAS   RE,PRTLINE          PRINT OUT DETAILS                            
*                                                                               
         USING SPAUTHD,R3                                                       
         LA    R3,ELEM             MOST IS ALREADY FILLED IN                    
         MVI   SPAFLAG,SPAFBUY                                                  
         MVC   SPASDTE,SASTDT      SET START DATE = GOOD ENOUGH                 
         MVC   SPAEDTE,SASTDT      AND END                                      
         MVI   SPAUPDT,SPAUPXFR    UPDATE NWS DATES                             
         MVC   SPAWRKDT,WRKADDDT   WORK REC ADDED DATE                          
         MVC   SPANWSDT,NWSXFRDT   NWS XFR DATE                                 
*                                                                               
         OC    SPAWRKDT,SPAWRKDT   IS THERE A WORK DATE                         
         BNZ   *+10                YES, THEN USE IT                             
         MVC   SPAWRKDT,SPANWSDT   NO, THEN USE NWS DATE IF ONE                 
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BNE   *+8                                                              
         OI    SPAFLAG,SPAFNOWR    TELL SPAUTH WRITE=NO                         
*                                                                               
         GOTO1 ASPAUTH,ELEM                                                     
         CLI   SPAERR,0                                                         
         BE    PT20                NEXT FLIGHT                                  
         DC    H'0'                                                             
*                                                                               
PTX      B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*===============================================================*               
*  PRINT DETAIL LINE - FLIGHT INFO AT R4                                        
*===============================================================*               
         USING SAUTHD,R4                                                        
PRTLINE  NTR1                                                                   
*                                                                               
         GOTO1 CLUNPK,DMCB,SRTCLT,PCLIENT                                       
         MVC   PPRD,QPRD                                                        
         MVC   PPRD2,QPRD2                                                      
         EDIT  (B1,SRTEST),PEST,FILL=0                                          
         GOTO1 MSUNPK,DMCB,LASTMSTA,PMKT,PSTAT                                  
         GOTO1 DATCON,DMCB,(2,SASTDT),(5,PSTFLT)                                
         GOTO1 DATCON,DMCB,(2,SAENDT),(5,PENDFLT)                               
*                                                                               
         OC    SANWSDT,SANWSDT                                                  
         BZ    PL10                                                             
         GOTO1 DATCON,DMCB,(2,SANWSDT),(5,PNWSDT)                               
PL10     OC    SAWRKDT,SAWRKDT                                                  
         BZ    PL20                                                             
         GOTO1 DATCON,DMCB,(2,SAWRKDT),(5,PWRKDT)                               
*                                                                               
PL20     GOTO1 REPORT                                                           
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*===============================================================*               
*  GET NWS DATA FROM BUY AND LOOK UP WORK RECORD ADDED DATE                     
*===============================================================*               
GETNWS   NTR1                                                                   
         XC    NWSXFRDT,NWSXFRDT   NWS XFER DATE                                
         XC    WRKADDDT,WRKADDDT   WORK REC ADDED DATE                          
*                                                                               
         USING BUYREC,R6                                                        
         L     R6,ADBUY            CHECK FOR NWS XFR ELEM                       
         LA    R6,BDELEM                                                        
GNWS10   CLI   0(R6),0                                                          
         BE    GNWSX                                                            
         CLI   0(R6),X'97'         NWS XFR ELEM                                 
         BE    GNWS20                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GNWS10                                                           
*                                                                               
         USING BWSELEM,R6                                                       
GNWS20   GOTO1 DATCON,DMCB,(3,BWSDATE),(2,NWSXFRDT)     XFR DATE                
         MVC   NWSBUYER,BWSBYR     SAVE BUYER                                   
         MVC   NWSCAMP,BWSCAM      SAVE CAMPAIGN                                
         XC    NWSCAMP,=X'FFFF'    COMPLIMENTED                                 
         DROP  R6                                                               
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT BUY KEY                         
         XC    KEY,KEY             AND READ NWS BUYER REC FOR CODE              
*                                                                               
         USING BYRRECD,R6                                                       
         LA    R6,KEY              NWS BUYER RECORD                             
         MVI   BYRKTYP,BYRKTYPQ    X'0D'                                        
         MVI   BYRKSUB,BYRKSUBQ    X'65'                                        
         MVC   BYRKAGMD,SRTAGMD                                                 
         MVC   BYRKBYR,NWSBUYER                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BNE   GNWSX10             RESTORE KEY AND EXIT                         
         LA    R1,IOAREA                                                        
         ST    R1,AREC                                                          
         GOTO1 GET                                                              
         LA    R6,IOAREA                                                        
         MVC   NWSBYRCD,BYRCODE    SAVE BUYER CODE                              
*                                                                               
         XC    KEY,KEY                                                          
         USING BWHRECD,R6                                                       
         LA    R6,KEY              NWS HEADER REC                               
         MVI   BWHKTYP,BWHKTYPQ    X'0D'                                        
         MVI   BWHKSUB,BWHKSUBQ    X'67'                                        
         MVC   BWHKAGMD,SRTAGMD                                                 
         MVC   BWHKBYR,NWSBYRCD    BUYER CODE                                   
         MVC   BWHKCAM,NWSCAMP     CAMPAIGN   <<--COMP??                        
         MVC   BWHKMKT,SVMKTSTA    CURRENT MARKET                               
         GOTO1 HIGH                                                             
         CLC   KEY(BWHKSEQ-BWHKEY),KEYSAVE   SAME THRU MKT                      
         BNE   GNWSX10                                                          
         GOTO1 GET                                                              
         LA    R6,IOAREA                                                        
         LA    R6,BWHEL            FIRST ELEM                                   
GNWS30   CLI   0(R6),0             EOR                                          
         BE    GNWSX10                                                          
         CLI   0(R6),X'06'         INFO ELEM                                    
         BE    GNWS40                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GNWS30                                                           
*                                                                               
         USING INFELD,R6                                                        
GNWS40   GOTO1 DATCON,DMCB,(3,INFADDED),(2,WRKADDDT)    WORK ADDED              
*                                                                               
GNWSX10  MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RESTORE SEQUENCE                             
*                                                                               
GNWSX    XIT1                                                                   
         DROP  R6                                                               
*===============================================================*               
*  CONVERT BINARY PRODUCT CODE TO MNEMONIC                                      
*  INPUT: BYTE = HEX CODE, OUTPUT: THREE = 3 BYTE MNEMONIC                      
*===============================================================*               
         USING CLTHDR,R4                                                        
CONVPRD  NTR1                                                                   
         L     R4,ADCLT                                                         
         LA    R3,CLIST            MATCH PRODUCT CODE IN LIST TO                
CPRD10   CLI   0(R3),0             GET 3 BYTE MNEMONIC                          
         BNE   *+6                                                              
         DC    H'00'               DIE IF NO MATCH                              
         CLC   3(1,R3),BYTE                                                     
         BE    CPRD20                                                           
         LA    R3,4(R3)                                                         
         B     CPRD10                                                           
CPRD20   MVC   THREE,0(R3)         SAVE 3 BYTE PRODUCT MNEMONIC                 
         B     EXIT                                                             
         DROP  R4                                                               
*===============================================================*               
*  READ ESTIMATE RECORD                                               *         
*===============================================================*               
READEST  NTR1                                                                   
         USING ESTHDR,R4                                                        
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   EKEYAM,SRTAGMD                                                   
         MVC   EKEYCLT,SRTCLT                                                   
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,SRTEST                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NO                                                               
***      BE    *+6                                                              
***      DC    H'00'                                                            
         GOTO1 GETEST                                                           
         L     R4,ADEST                                                         
         GOTO1 DATCON,DMCB,ESTART,(3,DUB)                                       
         MVC   SVFLSTD,DUB         SAVE ESTIMATE START DATE                     
         GOTO1 DATCON,DMCB,EEND,(3,DUB)                                         
         MVC   SVFLEND,DUB         SAVE ESTIMATE END DATE                       
         TM    EFLAG1,EF1SDE       SUPERDESK AUTH RECORD OPEN?                  
         BNO   NO                  IF NOT, SKIP                                 
*                                                                               
         OC    SRTPRD2,SRTPRD2     PIGGYBACK PRODUCT                            
         BZ    YES                                                              
         LA    R4,KEY                                                           
         MVC   EKEYPRD,QPRD2                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETEST                                                           
         L     R4,ADEST                                                         
         TM    EFLAG1,EF1SDE                                                    
         BNO   NO                                                               
         B     YES                                                              
         DROP  R4                                                               
*===============================================================*               
*  READ FLIGHT RECORD                                                           
*===============================================================*               
READFLT  NTR1                                                                   
         LA    R4,KEY                                                           
         USING DFLRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   DFLKTYP(2),=X'0D38'                                              
         MVC   DFLKAGMD,SRTAGMD                                                 
         MVC   DFLKCLT,SRTCLT                                                   
         MVC   DFLKPRD,=C'POL'                                                  
         MVC   DFLKEST,SRTEST                                                   
         GOTO1 HIGH                FIRST READ FOR PRD POL                       
         CLC   KEY(9),KEYSAVE                                                   
         BE    RDFLT10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(9),KEYSAVE      IF NO FLIGHT RECORD FOUND FOR                
         MVC   DFLKPRD,QPRD        POL THEN READ FOR BRAND                      
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   NO                                                               
*                                                                               
RDFLT10  L     R4,ADMARKET                                                      
         ST    R4,AREC                                                          
         GOTO1 GET                                                              
         LA    R4,DFLEL                                                         
RDFLT20  CLI   0(R4),X'00'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R4),X'05'                                                      
         BE    RDFLT30                                                          
RDFLT25  ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     RDFLT20                                                          
         USING DFFLTEL,R4                                                       
RDFLT30  CLC   DFFLTNUM,SRTFLT     FLIGHT NUMBER                                
         BNE   RDFLT25                                                          
         MVC   SVFLSTD,DFFLTSTR    SAVE FLIGHT START DATE                       
         MVC   SVFLEND,DFFLTEND    SAVE FLIGHT END DATE                         
RDFLTX   B     YES                                                              
         DROP  R4                                                               
*===============================================================*               
*  GET MARKET FROM STATION RECORD                                               
*===============================================================*               
GETMKT   NTR1                                                                   
*                                                                               
         MVC   BYTE,SRTAGMD        FIGURE OUT ALPHA MEDIA                       
         NI    BYTE,X'0F'          ISOLATE MEDIA                                
         LA    R1,MEDTAB                                                        
GETMKT02 CLC   BYTE,0(R1)                                                       
         BE    GETMKT04                                                         
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'         EOT                                          
         BNE   GETMKT02                                                         
         DC    H'0'                                                             
GETMKT04 MVC   QMED,1(R1)          ALPHA MEDIA                                  
*                                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,SRTCLT),QCLT                              
*                                                                               
         XC    BMKTSTA,BMKTSTA     GET STATION CALL LETTERS                     
         MVC   BSTA,SRTSTA                                                      
         GOTO1 MSUNPK,DMCB,BMKTSTA,QMKT,QSTA                                    
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
GETMKT05 XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING STAREC,R6                                                        
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,QSTA                                                    
         MVC   STAKAGY,SRTQAGY                                                  
         MVC   STAKCLT,QCLT                                                     
         L     R6,ADSTAT                                                        
         GOTO1 HIGHSTA                                                          
         CLC   KEY(STAKFILL-STAREC),STAREC                                      
         BE    GETMKT10                                                         
*                                                                               
         MVC   KEY+9,=C'000'     READ WITHOUT CLIENT                            
         GOTO1 HIGHSTA                                                          
         CLC   KEY(STAKFILL-STAREC),STAREC                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
GETMKT10 PACK  DUB,SMKT            MARKET NUMBER                                
         CVB   R3,DUB                                                           
         STCM  R3,3,BMKT                                                        
         B     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
         GETEL R4,24,ELCODE                                                     
*                                                                               
MEDTAB   DC    XL1'01',CL1'T'                                                   
         DC    XL1'02',CL1'R'                                                   
         DC    XL1'03',CL1'N'                                                   
         DC    XL1'04',CL1'X'                                                   
         DC    XL1'08',CL1'C'                                                   
         DC    XL1'FF'                                                          
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,17,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=17'                                    
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=6100,             X        
               MACRF=GM,EODAD=SU99                                              
*                                                                               
         EJECT                                                                  
ASAUTHTB DS    F                                                                
ASPAUTH  DS    A                                                                
CARD     DS    CL80                                                             
ELEM     DS    CL256                                                            
TODAYQ   DS    CL6                                                              
SVSENTDT DS    XL2                                                              
SVCONFDT DS    XL2                                                              
ELCODE   DS    X                                                                
TABLKEY  DS    XL13                                                             
LASTMSTA DS    XL5                                                              
NWSXFRDT DS    XL2                 NWS TRANSFER DATE                            
NWSBUYER DS    CL3                 NWS BUYER                                    
NWSBYRCD DS    XL1                 NWS BUYER CODE                               
NWSCAMP  DS    XL2                 NWS CAMPAIGN                                 
WRKADDDT DS    XL2                 WORK REC ADDED DATE                          
SVCPROF  DS    CL15                                                             
SVFLSTD  DS    CL3                                                              
SVFLEND  DS    CL3                                                              
SVAGMED  DS    X                                                                
SVMKTSTA DS    XL5                                                              
SVKEY    DS    XL50                                                             
SVQAGY   DS    CL2                                                              
SAVESRT  DS    CL(SRRECLEN)                                                     
SORTREC  DS    CL(SRRECLEN)        SORT RECORD                                  
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVRECLN DC    H'0'                                                             
         DC    H'0'                                                             
RCVREC   DS    0F                                                               
*PREFIX=R                                                                       
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
RKEY     DS    0CL13                                                            
IOAREA   DS    4000C                                                            
         DS    XL30                LEAVE ROOM FOR RECOVERY TRAILER              
SAUTHTAB DS    4000C                                                            
AUTHTAB  DS    2000C                                                            
         EJECT                                                                  
SAUTHD   DSECT                     STATION MISSING AUTH TABLE                   
SASTDT   DS    XL2                 FLIGHT START DATE                            
SAENDT   DS    XL2                 FLIGHT END DATE                              
SANWSDT  DS    XL2                 LATEST NWS XFR DATE                          
SAWRKDT  DS    XL2                 EARLIEST WORK ADDED DATE                     
SASPOTS  DS    XL1                 Y/N SPOTS FOUND = ADD AUTH                   
SALENQ   EQU   *-SAUTHD                                                         
*                                                                               
SRTRECD  DSECT                                                                  
SRTTYPE  DS    CL1                 TYPE OF SORT REC                             
SRTTDAR  EQU   C'D'                DARE RECORD FROM RECV FILE                   
SRTTAUT  EQU   C'A'                AUTH RECORD FROM RECV FILE                   
*                                                                               
SRTAGMD  DS    XL1                 AGY/MD                                       
SRTCLT   DS    XL2                 CLIENT                                       
SRTPRD   DS    XL1                 PRODUCT                                      
SRTPRD2  DS    XL1                 PRODUCT2                                     
SRTEST   DS    XL1                 ESTIMATE                                     
*                                                                               
SRTQAGY  DS    CL2                 ALPHA AGENCY                                 
*                                                                               
SRTFLT   DS    XL1                 FLIGHT    - ONLY FOR TYPE DARE               
SRTSTA   DS    XL3                 STATION   - ONLY FOR TYPE DARE               
SRTSENTD DS    XL2                 SENT DATE - ONLY FOR TYPE DARE               
SRTCONFD DS    XL2                 CONF DATE - ONLY FOR TYPE DARE               
*                                                                               
SRRECLEN EQU   *-SRTRECD                                                        
*                                                                               
*  PRINT LINE DSECT                                                             
       ++INCLUDE SPAUTHD                                                        
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENAUTH                                                      
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
PRDRECD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPNWSBYR                                                       
       ++INCLUDE SPNWSHDR                                                       
       ++INCLUDE SPGENDRFLT                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
         ORG   P                                                                
PMEDIA   DS    CL1                                                              
         DS    CL1                                                              
PCLIENT  DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PPRD2    DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PSTAT    DS    CL5                                                              
         DS    CL1                                                              
PSTFLT   DS    CL8                                                              
         DS    CL1                                                              
PENDFLT  DS    CL8                                                              
         DS    CL1                                                              
PNWSDT   DS    CL8                                                              
         DS    CL1                                                              
PWRKDT   DS    CL8                                                              
         DS    CL1                                                              
         DS    CL1                                                              
PSENTDT  DS    CL8                                                              
         DS    CL1                                                              
PCONFDT  DS    CL8                                                              
         DS    CL1                                                              
         ORG                                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPSU02 05/07/13'                                      
         END                                                                    
