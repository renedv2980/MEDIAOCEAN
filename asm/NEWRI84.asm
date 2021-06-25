*          DATA SET NEWRI84    AT LEVEL 052 AS OF 05/05/20                      
*PHASE T32084A,+0                                                               
*INCLUDE BRDMON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
         TITLE 'T32084 - MODULE TO READ INVOICE RECORDS'                        
***********************************************************************         
*                                                                     *         
*                 CALLED FROM NEWRI20 FOR NET WRITER                  *         
*                 ----------------------------------                  *         
*                                                                     *         
***********************************************************************         
* USER      JIRA        DATE                 CHANGE LOG               *         
* ----  ------------  --------  ------------------------------------- *         
* AKAT  SPEC-45745    05/05/20  HONOR NEGATIVE ITEM (SNVIDNGQ) FLAG   *         
* AKAT  CUSTENH-2936  06/16/16  NEW INVCHGDT & INVPID KEYWORD SUPPORT *         
*                                                                     *         
*  AKAT 11/02/15 049 - HONOR ESTIMATE FILTERS                         *         
*                                                                     *         
***********************************************************************         
T32084   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKLEN,T32084**,CLEAR=YES                                        
         USING T32084,RB,RA     RB,RA BASE REGISTERS                            
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LR    R7,RC                                                            
         USING WRKAREA,R7                                                       
         L     RC,0(R1)            *CALLING PROGRAMS RC                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,NDGLOBAL                                                      
         USING GLOBALD,R6                                                       
         LA    RF,HOOK             SET IN NEWRI20 AND RESET HERE SINCE          
         ST    RF,GLAHOOK          DRIVER IS CALLED FROM BOTH MODULES           
         LA    R1,INVDATA                                                       
         ST    R1,NDANINV          PASS ADDR OF TABLE TO DRIVER                 
         NETGO NVSETSPT,DMCB       SET FOR SPOTFILE                             
         B     INT00                                                            
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
INT00    DS    0H                                                               
*  INITIALIZE *                                                                 
*                                                                               
         XC    INVRDCLI,INVRDCLI         * CLIENT                               
         CLI   NBSELCLI,0                                                       
         BE    INT01                                                            
         CLC   =C'ALL',NBSELCLI                                                 
         BE    *+10                                                             
         MVC   INVRDCLI,NBACTCLI                                                
                                                                                
*                                         * GET PACKED STATION                  
INT01    XC    INVRDSTA,INVRDSTA                                                
         CLI   NBSELNET,0          ANY STATIONS?                                
         BE    INT10                                                            
         CLC   =C'ALL',NBSELNET    ALL STATIONS?                                
         BE    INT10                                                            
         MVI   FULL,C'0'           PASS ZERO FOR MARKET                         
         MVC   FULL+1(3),FULL                                                   
         MVC   DUB(4),NBSELNET                                                  
         MVI   DUB+4,C'N'          N FOR NETWORK                                
         GOTO1 MSPACK,DMCB,FULL,DUB,WORK                                        
         MVC   INVRDSTA,WORK+2     SAVE 3 BYTE PACKED STATION                   
*                                                                               
                                                                                
* - GET BROADCAST START-END OF REQUEST TO INVRDSDT AND INVRDEDT                 
INT10    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,NBSELSTR),(2,INVRDSDT)                            
         GOTO1 DATCON,DMCB,(0,NBSELEND),(2,INVRDEDT)                            
                                                                                
         L     R1,ANETWS1          SAVE NETIO CLIENT REC                        
         MVC   CLTRECSV,0(R1)                                                   
* END INITIALIZATION                                                            
                                                                                
         BAS   RE,RNEW                READ NEW INVOICE RECORDS                  
                                                                                
         BAS   RE,RESTCLT          RESTORE NETIO CLIENT                         
         XC    INVDATA,INVDATA      CLEAR TABLE                                 
         XC    FILENAME,FILENAME    CLEAR                                       
         NETGO NVSETUNT,DMCB        RESET FOR UNIT FILE                         
         B     EXIT                                                             
*                                                                               
RNEW     NTR1                           NEW INVOICE RECORD                      
*                                       --- ------- -------                     
                                                                                
*                                  FIRST SET START-END MOS                      
         MVC   INVSTMOS,=X'FFFF'                                                
         XC    INVENMOS,INVENMOS                                                
         OC    INVRDSDT,INVRDSDT                                                
         BZ    RN51                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVRDSDT),(0,DUB)                                 
         MVC   DUB+4(2),=C'01'         1ST OF MONTH                             
         GOTO1 DATCON,(R1),(0,DUB),(2,WORK)                                     
         MVC   INVSTMOS,WORK                                                    
         XC    INVSTMOS,=X'FFFF'     COMPLEMENT                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVRDEDT),(0,DUB)                                 
         MVC   DUB+4(2),=C'01'         1ST OF MONTH                             
         GOTO1 DATCON,(R1),(0,DUB),(2,WORK)                                     
         MVC   INVENMOS,WORK                                                    
         XC    INVENMOS,=X'FFFF'     COMPLEMENT                                 
*                                                                               
RN51     BAS   RE,CLEARF1          CLEAR THE SAVED X'F1' ELEMENT                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING SNVKEYD,R2                                                       
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,NBACTAM                                                   
         MVC   SNVKCLT,INVRDCLI                                                 
         MVC   SNVKSTA,INVRDSTA                                                 
*                                                                               
RN52     MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH                                                             
*                                                                               
RN54     CLC   SNVKEY(3),KEYSAVE     CHECK AGENCY/MEDIA                         
         BNE   RNX                                                              
*                                                                               
* - CLIENT GROUP FILTERS                                                        
         CLI   NBSELCGR,0                                                       
         BE    RN54C                                                            
         BAS   RE,CGRPFILT         CHECK GROUP FILTER                           
         BE    RN55                OK                                           
         B     RN70                NO-SKIP TO NEXT CLIENT                       
                                                                                
* - CLIENT OFFICE FILTERS                                                       
RN54C    CLI   NBSELOFF,0          CLIENT OFFICE FILTER?                        
         BE    RN54X                                                            
         BAS   RE,COFFFILT         CHECK CLIENT OFFICE FILTER                   
         BE    RN55                YES                                          
         B     RN70                NO-SKIP TO NEXT CLIENT                       
                                                                                
* - CLIENT FILTER                                                               
RN54X    DS    0H                                                               
         OC    INVRDCLI,INVRDCLI    ALL CLIENTS - THEN ACCEPT ALL               
         BNZ   RN54Y               NO                                           
         BAS   RE,CHKCLT           CHECK CLIENT                                 
         B     RN55                                                             
*                                                                               
RN54Y    DS    0H                                                               
         OC    SNVKCLT,INVRDCLI    CLIENT FILTER?                               
         BZ    RN55                 NO                                          
         CLC   SNVKCLT,INVRDCLI     YES/CHECK CLIENT                            
         BE    RN55                                                             
         BH    RNX                 HI-FINISHED                                  
         B     RN70                LOW-SKIP TO NEXT CLIENT                      
                                                                                
RN55     CLC   LASTCLT,SNVKCLT     DID CLIENT CHANGE?                           
         BE    RN55AA              NO                                           
         BAS   RE,SETEST           YES - SET ESTIMATE MASK                      
         MVC   LASTCLT,SNVKCLT     SAVE THE CLIENT WE JUST PROCESSED            
*                                                                               
* STATION TYPE FILTER                                                           
*                                                                               
* - STATION GROUP FILTERS                                                       
RN55AA   CLI   NBSELNGR,0                                                       
         BE    RN55A                                                            
         XC    DUB,DUB                        STATION                           
         MVC   DUB+2(3),SNVKSTA                                                 
         GOTO1 MSUNPK,DMCB,DUB,FULL,WORK                                        
         BAS   RE,SGRPFILT         CHECK GROUP FILTER                           
         BE    RN55YES             OK                                           
         B     RN55NO              NO-SKIP TO NEXT CLIENT                       
*                                                                               
RN55A    DS    0H                                                               
* CHANGE BELOW TO ALWAYS FILL IN NBEFFMED                                       
***      CLI   NBSELMFL,0          FILTERING                                    
***      BE    RN55YES             NO                                           
         L     R3,NBANBUFF         YES/BUFFER AROUND?                           
         MVI   NBEFFMED,0                                                       
         LTR   R3,R3                                                            
         BZ    RN55YES             NO FILTER                                    
         XC    DUB,DUB                        STATION                           
         MVC   DUB+2(3),SNVKSTA                                                 
         GOTO1 MSUNPK,DMCB,DUB,FULL,WORK                                        
         USING NBUFFREC,R3                                                      
         LA    R3,L'NBUFFREC(R3)   FIRST ENTRY HAS PRESENT CLIENT               
         LA    R0,1199             MAX ENTRIES IN STATION TABLE                 
*                                                                               
RN55B    OC    NBUFFREC,NBUFFREC   LOOK UP IN NETWORK BUFFER                    
         BZ    RN55E                                                            
         CLC   NBUFFNET,WORK       FOR A MATCH ON NETWORK                       
         BE    RN55D                                                            
         LA    R3,L'NBUFFREC(R3)                                                
         BCT   R0,RN55B                                                         
         B     RN55E                                                            
*                                                                               
RN55D    MVC   NBEFFMED,NBUFFMED   GOT A HIT SO PICK UP MEDIA                   
         DROP  R3                                                               
*                                                                               
RN55E    CLI   NBSELMFL,0          FILTERING ???                                
         BE    RN55YES             NO                                           
         TM    NBSELMFL,X'40'                                                   
         BNO   RN55F                                                            
         CLC   NBSELMFL,NBEFFMED                                                
         BE    RN55YES                                                          
         B     RN55NO                                                           
*                                                                               
RN55F    MVC   BYTE,NBSELMFL       NEGATIVE FILTER                              
         OI    BYTE,X'40'                                                       
         CLC   BYTE,NBEFFMED                                                    
         BE    RN55NO                                                           
         B     RN55YES                                                          
RN55YES  B     RN56                                                             
RN55NO   B     RN68                ADVANCE TO NEXT STATION                      
*                                                                               
RN56     OC    INVRDSTA,INVRDSTA   TEST FOR STATION FILTER                      
         BZ    RN58                                                             
         CLC   SNVKSTA,INVRDSTA    YES-CHECK THE STATION                        
         BE    RN62                                                             
         BH    RN70                HI-SKIP TO NEXT CLIENT                       
         B     RN68                LO-ADVANCE TO STATION                        
*                                                                               
RN58     DS    0H                                                               
*                                                                               
RN62     OC    INVENMOS,INVENMOS   TEST HAVE A END MOS                          
         BZ    RN64                                                             
         CLC   SNVKMOS,INVENMOS    YES-TEST MONTH AFTER END                     
         BNL   RN64                                                             
         MVC   SNVKMOS,INVENMOS    YES-ADVANCE TO END                           
         XC    SNVKINV(20),SNVKINV                                              
         B     RN52                                                             
*                                                                               
RN64     CLC   INVSTMOS,XFF        TEST HAVE A START MONTH                      
         BE    RN80                                                             
         CLC   SNVKMOS,INVSTMOS    YES-TEST BEFORE START MONTH                  
         BNH   RN80                                                             
*                                                                               
RN68     DS    0H                  NEXT STATION                                 
         MVC   SNVKMOS,XFF                                                      
         B     RN52                                                             
*                                                                               
RN70     MVC   SNVKSTA(3),XFF      SKIP TO NEXT CLIENT                          
         XC    SNVKINV(20),SNVKINV                                              
         B     RN52                                                             
*                                                                               
RN80     DS    0H                                                               
*                                                                               
RN90     MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,KEY                                                           
         USING SNVKEYD,R2                                                       
*                                                                               
         TM    NDINDS2,ND2ILAST     REPORTING INVCHGDT/INVPID KEYWORDS?         
         BZ    RN90A                NO                                          
         BAS   RE,TESTF1            SAVED X'F1' ELEMENT SET?                    
         BNZ   RN90A                YES                                         
         CLC   SNVKMINK,XFF         IS THIS KEY THE LAST MINIO RECORD?          
         BE    RN90A                YES - USE THE RECORDS X'F1' ELEMENT         
         CLC   MYKEYSV(NOMINIO),KEY ALREADY PROCESSED?                          
         BE    RN90A                YES - ALREADY SET                           
*                                                                               
         BAS   RE,LASTINV           GET THE LAST INV REC IN MINIO SET           
*                                                                               
RN90A    GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL  ',SNVDDA,MYIO,MYDM            
*                                                                               
         BAS   RE,NEWINVRC         PROCESS THE INVOICE RECORD                   
*                                                                               
         CLI   NEWMINRC,C'Y'       SKIP TO NEW MINIO REC                        
         BNE   RN90Z               NO/GET NEXT MINIO REC                        
         MVI   NEWMINRC,0          CLEAR FLAG                                   
         CLC   SNVKMINK,XFF        NO MORE MINOIO                               
         BE    RN90Z                                                            
         BAS   RE,CLEARF1          CLEAR THE SAVED X'F1' ELEMENT                
         MVC   SNVKMINK,XFF                                                     
         MVI   SNVKMINK+(L'SNVKMINK),1                                          
         B     RN52                                                             
*                                                                               
RN90Z    GOTO1 SEQ                  READ SEQ                                    
*                                                                               
         CLC   KEY(NOMINIO),KEYSAVE SAME INVOICE AS LAST TIME?                  
         BE    RN54                 YES - READ NEXT MINIO RECORD SET            
         BAS   RE,CLEARF1           NO - CLEAR THE SAVED X'F1' ELEMENT          
         MVI   INVBREAK,C'Y'                                                    
         B     RN54                                                             
         DROP  R2                                                               
*                                                                               
RNX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
CLEARF1  DS    0H                   CLEAR THE SAVED COPY OF X'F1' ELEM          
         L     R3,=A(ELEMF1)        SAVED COPY OF X'F1' ELEMENT                 
         XC    0(L'ELEMF1,R3),0(R3) CLEAR                                       
         BR    RE                   RETURN                                      
*                                                                               
TESTF1   DS    0H                   SEE IF SAVED X'F1' ELEMENT IS SET           
         L     R1,=A(ELEMF1)        SAVED COPY OF X'F1' ELEMENT                 
         OC    0(L'ELEMF1,R1),0(R1) ALREADY HAVE THE SAVED X'F1' ELEM?          
         BR    RE                   RETURN WITH CC SET                          
*                                                                               
LASTINV  NTR1                                                                   
*                                                                               
         LA    R2,KEY               A(INVOICE KEY)                              
         USING SNVKEYD,R2           INVOICE KEY DSECT                           
*                                                                               
         MVC   SNVKMINK,XFF         READ LAST MINIO RECORD                      
*                                                                               
         GOTO1 HIGH                 READ HIGH                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL  ',SNVDDA,MYIO,MYDM            
*                                                                               
         LA    R2,MYIO              A(INVOICE RECORD)                           
         LA    R3,SNVELS            START OF ELEMENTS                           
         XR    RE,RE                CLEAR RE                                    
*                                                                               
LINV10   CLI   0(R3),0              END OF RECORD?                              
         BE    LINV30               YES - DONE                                  
         CLI   0(R3),X'F1'          ACTIVITY ELEMENT?                           
         BE    LINV20               YES - COPY TO ELEMF1                        
*                                                                               
         IC    RE,1(R3)             ELEMENT LENGTH                              
         AR    R3,RE                BUMP TO NEXT ELEMENT                        
         B     LINV10               CHECK NEXT ELEMENT                          
*                                                                               
LINV20   L     R1,=A(ELEMF1)        A(ELEMF1)                                   
         MVC   0(L'ELEMF1,R1),0(R3) SAVE X'F1' ACTIVITY ELEMENT                 
*                                                                               
LINV30   LA    R2,KEY               A(INVOICE KEY)                              
         XC    SNVKMINK,SNVKMINK    READ FIRST MINIO RECORD                     
*                                                                               
         GOTO1 HIGH                 READ HIGH TO RESET READ SEQ                 
*                                                                               
         B     EXIT                 EXIT                                        
         DROP  R2                   DROP USINGS                                 
*                                                                               
NEWINVRC NTR1                                                                   
*                                                                               
         LA    R8,INVDATA          R8->INVOICE BLOCK                            
         USING NETINVD,R8                                                       
         CLI   INVBREAK,C'Y'                                                    
         BNE   *+10                                                             
         XC    0(NETINVBE,R8),0(R8)      CLEAR INVOICE BLOCK                    
         MVI   INVBREAK,C'N'       RESET SWITCH                                 
         LA    R3,MYIO             R3->INVOICE KEY                              
         USING SNVKEY,R3                                                        
* CLEAR SAVED 30 ELEMS AT NEW MINIO REC                                         
         CLC   MYKEYSV(SNVKMINK-SNVKTYPE),KEY                                   
         BE    INV01               SAME MINIO REC                               
         MVC   MYKEYSV,KEY         NO-SAVE KEY FOR MINIO REC CHECK              
*                                                                               
         L     RE,=A(ELEM30S)      SNEW REC CLEAR X'30'SAVE AREA                
         LA    RF,ELEM30E                                                       
         XCEF                                                                   
*                                                                               
INV01    DS    0H                                                               
*                                                                               
         MVC   IAMSV,SNVKAM                   AGENCY/MEDIA                      
         MVC   CLTSV,SNVKCLT                  CLIENT  (2 BYTES)                 
         XC    DUB,DUB                        STATION                           
         MVC   DUB+2(3),SNVKSTA                                                 
         GOTO1 MSUNPK,DMCB,DUB,FULL,WORK                                        
         MVC   STASV,WORK                                                       
*                                                                               
         MVC   NETISTA,WORK                    STATION                          
         MVC   NETIAM,SNVKAM                   AGENCY/MEDIA                     
         MVC   NETICLT,SNVKCLT                 CLIENT  (2 BYTES)                
         MVC   NETIINO,SNVKINV                INVOICE NUMBER                    
         MVC   NETINSV,SNVKINV                INVOICE NUMBER                    
*                                                                               
         LA    R3,SNVELS-SNVKEY(R3)    BUMP TO START OF ELEMENTS                
         B     INV4                                                             
*                                                                               
INV2     ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
INV4     CLI   0(R3),0             SEARCH RECORD FOR INVOICE ELEMENTS           
         BE    INVX                                                             
         CLI   0(R3),X'10'         HEADER ELEM?                                 
         BE    INV5                                                             
         CLI   0(R3),X'40'         DETAIL                                       
         BE    INV6                                                             
         CLI   0(R3),X'30'         FIRST 30 ?                                   
         BNE   INV2                                                             
*                                                                               
         L     RE,=A(ELEM30S)                                                   
         LA    RF,ELEMNUM               NUMBER OF ELEMS                         
*                                                                               
INV4D    MVC   0(SNVCMLNQ,RE),0(R3)                                             
         LA    RE,SNVCMLNQ(RE)     BUMP AREA                                    
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'30'                                                      
         BNE   INV4                NO                                           
         BCT   RF,INV4D                                                         
         DC    H'0'                EXPAND TABLE                                 
*                                                                               
         USING SNVHDEL,R3                                                       
*                                                                               
INV5     DS    0H                                                               
         XC    0(NETINVBE,R8),0(R8)      CLEAR INVOICE BLOCK                    
         XC    HEDEST(HEDLENE),HEDEST    CLEAR HEADER SAVE AREA                 
         XC    PERDATSV,PERDATSV                                                
                                                                                
*  INVOICE PERIOD WITHIN REQUEST DATES?                                         
         CLC   SNVHDSDT,INVRDEDT   INV PERIOD START > REQUEST END DATE?         
*****    BH    INVX                YES/SKIP INVOICE                             
         BH    EASI12              YES/SKIP INVOICE                             
         CLC   SNVHDEDT,INVRDSDT   INV PERIOD END < REQUEST START DATE?         
*****    BL    INVX                YES/SKIP INVOICE                             
         BL    EASI12              YES/SKIP INVOICE                             
*                                                                               
         TM    NBACCFLT,X'18'          EASI FILTERING?                          
         BZ    EASIEND                                                          
         OC    SNVHDEZS,SNVHDEZS       EASI ?                                   
         BNZ   EASI10                  YES                                      
         TM    NBACCFLT,X'10'          NO/ONLY EASI?                            
         BO    EASI12                  YES/SKIP                                 
         B     EASIEND                                                          
EASI10   TM    NBACCFLT,X'08'          SKIP EASI?                               
         BO    EASI12                  YES-SKIP                                 
         B     EASIEND                                                          
EASI12   MVI   NEWMINRC,C'Y'       SET FLAG TO GET NEW MINIO REC                
         B     INVX                                                             
EASIEND  EQU   *                                                                
*                                                                               
*USE INV PERIOD STATR DATE TO GET MOS                                           
         GOTO1 DATCON,DMCB,(2,SNVHDSDT),(0,WORK)                                
         MVC   PERDATSV,WORK                                                    
         GOTO1 =V(BRDMON),DMCB,WORK,WORK                                        
         GOTO1 DATCON,DMCB,(0,WORK),(2,INDATSV)                                 
*                                                                               
         MVC   HEDEST,SNVHDEST            ESTIMATE = 0 = MIXED                  
         MVC   HEDPRD,SNVHDPRD            PROD = X'FF' = VARIOUS                
         MVC   HEDPRD2,SNVHDPR2           PROD2                                 
         CLI   SNVHDLEN,SNVHDLN3   DO WE HAVE ALPHA PRODS?                      
         BNE   *+16                                                             
         MVC   HEDPRDA,SNVHDAP1    YES-SET THEM                                 
         MVC   HEDPRDA2,SNVHDAP2                                                
                                                                                
         XC    DATSV,DATSV                                                      
         XC    DUDATSV,DUDATSV                                                  
         OC    SNVHDIDT,SNVHDIDT       INVOICE DATE?                            
         BZ    INV5A                                                            
         GOTO1 DATCON,DMCB,(2,SNVHDIDT),(5,DATSV)                               
                                                                                
INV5A    OC    SNVHDDDT,SNVHDDDT       INVOICE DUE DATE?                        
         BZ    INV5B                                                            
         GOTO1 DATCON,DMCB,(2,SNVHDDDT),(5,DUDATSV)                             
                                                                                
INV5B    DS    0H                                                               
         MVC   NETIAM,IAMSV        SET AGENCY/MEDIA                             
         MVC   NETIINO,NETINSV      SET INVOICE NUMBER                          
         MVC   NETICLT,CLTSV       SET CLIENT                                   
         MVC   NETISTA,STASV       SET STATION                                  
         MVC   NETIDAT,DATSV       DATE                                         
         MVC   NETIDDAT,DUDATSV    DUE DATE                                     
         MVC   NETICDAT,SNVHDCDT   CREATION DATE                                
         MVC   NETIPRD,SNVHDPRD    PROD                                         
         MVC   NETIPRD2,SNVHDPR2   PROD2                                        
         MVC   NETIPRDC,HEDPRDA    ALPHA PROD                                   
         MVC   NETIP2C,HEDPRDA2    ALPHA PROD2                                  
         MVC   NETIEST,SNVHDEST    EST                                          
         MVC   NETIHCST,SNVHDTCS   TOTAL COST                                   
         MVC   NETIHSPT,SNVHDTSP   TOTAL SPOTS                                  
         MVC   NETISRCE,SNVHDEZS   EASI SOURCE                                  
                                                                                
*   GET ALPHA PROD IF NECESSARY *                                               
                                                                                
         CLI   NETIPRD,0           IF BINARY ?                                  
         BE    INV5D               NO                                           
         CLI   NETIPRDC,0          DO WE HAVE ALPHA?                            
         BNE   INV5C               YES                                          
         MVC   WORK(1),NETIPRD     NO                                           
         BAS   RE,GETPRD3          GET IT                                       
         MVC   NETIPRDC,WORK       SET IT                                       
*                                                                               
INV5C    CLI   NETIPRD2,0          2ND PROD?                                    
         BE    INV5D                                                            
         CLI   NETIP2C,0           DO WE HAVE ALPHA?                            
         BNE   INV5D                                                            
         MVC   WORK(1),NETIPRD2    YES                                          
         BAS   RE,GETPRD3          GET ALPHA                                    
         MVC   NETIP2C,WORK                                                     
*                                                                               
INV5D    CLI   NETIEST,0           IF NO EST                                    
         BE    INV2                SKIP HEADER                                  
         CLI   NETIPRD,0           IF NO PRODUCT                                
         BNE   INV5E                                                            
         CLI   NETIPRDC,0          ALPHA PROD ?                                 
         BE    INV2                NO-SKIP                                      
         BNE   INV5E               YES-CONTINUE                                 
*                                                                               
INV5E    BAS   RE,CHKFLTR                                                       
         BE    INV20               PASSED FILTER TEST                           
         BNE   INV2                SKIP HEADER/ GET DETAILS                     
**********************************************************                      
* INVOICE ITEM ELEMENT X'40'                                                    
         USING SNVIDEL,R3                                                       
INV6     DS    0H                                                               
*****    XC    0(NETINVBE,R8),0(R8)  SHOULD NOT BE CLEARED                      
*****                                KEY/HEADER INFO CARRIED OVER               
*****                                HEADER $ CLEARED BELOW AND                 
*****                                DETAIL DOLLARS MOVED IN BELOW              
*                                                                               
         ZAP   NETIHCST,=P'0'      TOTAL HEADER COST                            
         XC    NETIHSPT,NETIHSPT   TOTAL HEADER SPOTS                           
         XC    NETIFLM1,NETIFLM1                                                
         XC    NETIFLM2,NETIFLM2                                                
*                                                                               
         TM    NBINDS8,NBI8IMIR    SHIP MIRROR GHOST ELEMS?                     
         BNO   *+12                                                             
         TM    SNVIDFLG,SNVIDMIR   MIRROR GHOST UNIT ?                          
         BO    INV2                SKIP TO NEXT ELEMENT                         
*                                                                               
                                                                                
         MVC   NETIPRD,HEDPRD            PRODUCT FROM SAVED HEADER ELEM         
         MVC   NETIPRD2,HEDPRD2                                                 
         MVC   NETIPRDC,HEDPRDA                                                 
         MVC   NETIP2C,HEDPRDA2                                                 
                                                                                
         CLI   SNVIDPRD,0                DETAIL ELEM HAS PRODS ?                
         BE    *+10                                                             
         MVC   NETIPRD,SNVIDPRD          YES PROD1                              
         CLI   SNVIDPR2,0                                                       
         BE    *+10                                                             
         MVC   NETIPRD2,SNVIDPR2         YES  PROD2                             
                                                                                
*                              --->  NOTE BINARY AND ALPHA PRODS                
*                              --->  FROM HEADER/DETAIL                         
*                              --->  WILL THEY ALWAYS MATCH????                 
*                              --->  RELATION BETWEEN HEAD/DETAIL??             
         CLI   SNVIDLEN,SNVIDL3Q         ALPHA PRODS ?                          
         BE    INV7                      YES                                    
*                                                                               
         CLI   NETIPRD,0                 NO-GET THEM MYSELF                     
         BE    INV6C                                                            
         MVC   WORK(1),NETIPRD                                                  
         BAS   RE,GETPRD3                                                       
         MVC   NETIPRDC,WORK                                                    
*                                                                               
INV6C    CLI   NETIPRD2,0                                                       
         BE    INV8                                                             
         MVC   WORK(1),NETIPRD2                                                 
         BAS   RE,GETPRD3                                                       
         MVC   NETIP2C,WORK                                                     
         B     INV8                                                             
*                                                                               
INV7     CLI   SNVIDAP1,X'40'                                                   
         BNH   *+10                                                             
         MVC   NETIPRDC,SNVIDAP1                                                
         CLI   SNVIDAP2,X'40'                                                   
         BNH   *+10                                                             
         MVC   NETIP2C,SNVIDAP2                                                 
INV8     EQU   *                                                                
                                                                                
         MVC   NETIEST,HEDEST            ESTIMATE FROM HEADER ELEMENT           
         CLI   SNVIDEST,0                SPECIFICE ITEM ESTIMATE ?              
         BE    *+10                                                             
         MVC   NETIEST,SNVIDEST          YES                                    
*                                                                               
         BAS   RE,CHKFLTR          FILTERS MATCH ?                              
         BNE   INV2                NO - GET NEXT DETAIL ELEMENT                 
*                                                                               
         LA    R1,NETIFDT          DATA THAT IS ONLY ON LAST MINIO REC          
         XC    0(NETLEN1,R1),0(R1) CLEAR THIS DATA                              
*                                                                               
INV8A    DS    0H                                                               
*                                                                               
         LR    R1,R3                                                            
*                                                                               
INV9     ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         CLI   0(R1),0                                                          
         BE    INV12                                                            
         CLI   0(R1),X'E9'         I2 REQUEST DETAILS                           
         BE    INV10                                                            
         CLI   0(R1),X'EA'         I2 MATCHING INVOICE INFO                     
         BE    INV11                                                            
         CLI   0(R1),X'F1'         I2 ACTIVITY ELEMENT                          
         BE    INV11A                                                           
         B     INV9                                                             
*                                                                               
INV10    DS    0H                                                               
         USING SNVMTELD,R1                                                      
         MVC   NETIACM,SNVMTACM    ASSIGNED CMML MATCH                          
         B     INV9                                                             
         DROP  R1                                                               
*                                                                               
INV11    DS    0H                                                               
         USING SNVMRELD,R1                                                      
         MVC   NETIFDT,SNVMRFDT    FIRST INVOICE MATCH DATE                     
         MVC   NETIFPC,SNVMRFPC    FIRST MATCH %                                
         MVC   NETILDT,SNVMRLDT    LAST INVOICE MATCH DATE                      
         MVC   NETILPC,SNVMRLPC    LAST MATCH %                                 
         B     INV9                                                             
         DROP  R1                                                               
*                                                                               
INV11A   DS    0H                                                               
         USING ACTVD,R1                                                         
         MVC   NETILCD,ACTVCHDT    LAST CHANGE DATE                             
         MVC   NETILCP,ACTVCHID    LAST CHANGE PID                              
         B     INV9                                                             
         DROP  R1                                                               
*                                                                               
INV12    ICM   R1,15,SNVIDCST      INVOICE DETAIL COST                          
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                 NO                                           
         LNR   R1,R1               YES                                          
         STCM  R1,15,NETICOST      SET COST                                     
*                                                                               
         MVC   NETIINTG,SNVIDINT           SET INTEGRATION                      
         MVI   NETISPT,1                   SET SPOTS                            
*                                                                               
         OC    PERDATSV,PERDATSV                                                
         BZ    SKIPADAY                                                         
         ZIC   R4,SNVIDDAY        DAY RELATIVE TO FIRST DAY OF PERIOD           
         GOTO1 ADDAY,DMCB,PERDATSV,NETIUNDT,(R4)                                
SKIPADAY EQU   *                                                                
*                                                                               
         MVC   NETIUTIM,SNVIDTIM   TIME OF DAY IN MIN 0=6AM                     
         MVC   NETIUNLN,SNVIDSLN   SPOT LEN                                     
* FILM CODE                                                                     
         MVI   HALF,0                                                           
         L     R1,=A(ELEM30S)      SAVED ADDR OF X'30'                          
         LTR   R1,R1                                                            
         BZ    INV20                                                            
         USING SNVCMEL,R1                                                       
         CLI   SNVIDCML,0          FILM CODE FOR 1ST PROD                       
         BE    INV18                                                            
         MVC   BYTE,SNVIDCML       FILM INDEX TO BYTE                           
         LA    RE,NETIFLM1         1ST PROD OUT FIELD                           
INV13    CLC   BYTE,SNVCMICD                                                    
         BE    INV15                                                            
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         CLI   0(R1),X'30'                                                      
         BE    INV13                                                            
         B     INV18                                                            
INV15    MVC   0(12,RE),SNVCMCD                                                 
*                                                                               
INV18    CLI   HALF,X'FF'          EOF                                          
         BE    INV20                                                            
         CLI   SNVIDCM2,0          2ND FILM CODE?                               
         BE    INV20                                                            
         L     R1,ADR30S           RESET START OF X'30'S                        
         LA    RE,NETIFLM2         2ND PROD OUT FIELD                           
         MVC   BYTE,SNVIDCM2       2ND PROD INDEX                               
         MVI   HALF,X'FF'          EOF FLAG                                     
         B     INV13                                                            
         DROP  R1                                                               
         EJECT                                                                  
****************************************************************                
* HANDLE $$ HERE                                                                
*                                                                               
INV20    DS    0H                                                               
*                                                                               
         BAS   RE,TESTF1           SAVED X'F1' ELEMENT SET?                     
         BZ    INV20A              NO                                           
         USING ACTVD,R1            YES - R1 POINTS TO ELEMF1                    
         MVC   NETILCD,ACTVCHDT    LAST CHANGE DATE                             
         MVC   NETILCP,ACTVCHID    LAST CHANGE PID                              
         DROP  R1                  DROP USING                                   
*                                                                               
INV20A   MVC   NETIFILM,NETIFLM1   DEFAULT IS 1ST PROD                          
         CLC   NBSELPRD,=C'POL'    SPLITTING COSTS?                             
         BE    INV60               NOT FOR POL                                  
                                                                                
         MVI   MYBYTE,0            CLEAR SPLITTING COST FLAG                    
         TM    NBSPLOPT,X'80'      SPLITTING PRODUCTS                           
         BZ    INV60               NO                                           
         CLI   NETIPRD2,0          ONLY ONE PRODUCT?                            
         BNE   INV20X              NO - SPLIT                                   
         CLI   NETIP2C,0           ALPHA?                                       
         BE    INV60               NO-NO NEED TO SPLIT                          
INV20X   DS    0H                                                               
******************************************************                          
         CLI   0(R3),X'40'         ITEM ELEMENT?                                
         BNE   INV30                                                            
         MVC   COSTSV,NETICOST     SAVE TOTAL ITEM COST                         
         MVI   MYBYTE,1            SET SPLIT FLAG                               
         ICM   R0,15,NETICOST      GET COST INTO R0                             
         SRA   R0,1                THIS DIVIDES BY 2                            
         STCM  R0,15,NETICOST      SET 50% OF COST FOR THIS PROD                
         MVC   NETIFILM,NETIFLM1   SET 1ST PROD FILM                            
         B     INV40                                                            
*                                                                               
INV30    CLI   0(R3),X'10'        HEADER ELEMENT                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   COSTSV2,NETIHCST    SAVE TOTAL ITEM COST                         
         MVI   MYBYTE,1            SET SPLIT FLAG                               
         ZAP   PAKWRK,COSTSV2                                                   
         DP    PAKWRK,=P'2'        DIVIDE COST BY 2                             
         ZAP   NETIHCST,PAKWRK(8)   AND PASS 1/2 OF COST                        
         B     INV40                                                            
*                                                                               
INV40    DS    0H                  ARE WE LOOKING FOR SPECIFIC PROD?            
*                                                                               
         CLI   NBEFFPN3,0          LOOK AT ALPHA FIRST                          
         BE    INV60                                                            
         CLC   NBEFFPN3,NETIPRDC   ALPHAS MATCH?                                
         BE    INV60               YES                                          
         BNE   INV55               NO-CHECK PRD2                                
*                                                                               
INV50    CLI   NBEFFPNM,0          CHECK BINARY PROD ?                          
         BE    INV60               NO                                           
         CLC   NBEFFPNM,NETIPRD    DOES IT MATCH PROD1?                         
         BE    INV60               YES                                          
         B     INV55               NO/LET'S DEAL WITH PROD2                     
                                                                                
********???? NOTE PROD1 MATCH GOES TO INV60                                     
********???? NOTE PROD2 MATCH GOES TO INV57                                     
********???? IS THAT RIGHT ???????????                                          
                                                                                
*                                  DEAL WITH 2ND PRODUCT                        
INV55    DS    0H                                                               
         CLI   NBEFFPN3,0          APLPHA PROD?                                 
         BE    INV57                                                            
         CLC   NBEFFPN3,NETIP2C    ALPHAS MATCH                                 
         BE    INV57                                                            
         B     INV70                                                            
*                                                                               
INV57    CLI   0(R3),X'40'                SPECIFIC ITEM ELEM                    
         BNE   INV58                                                            
         ICM   R0,15,COSTSV               COST FOR BOTH PRODS                   
         ICM   R1,15,NETICOST             COST FOR PROD1                        
         SR    R0,R1                      SUBTRACT PROD1 FROM BOTH              
         STCM  R0,15,NETICOST             PASS COST FOR PROD2                   
         B     INV59                                                            
*                                                                               
INV58    CLI   0(R3),X'10'                HEADER ELEMENT                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   PAKWRK,COSTSV2             COST FOR BOTH PRODS                   
         SP    PAKWRK,NETIHCST            COST FOR PROD1                        
         ZAP   NETIHCST,PAKWRK                                                  
         B     INV59                                                            
*                                                                               
INV59    MVC   NETIPRD,NETIPRD2          SET 2ND PROD TO PROD                   
*                                                                               
         MVC   NBSPLPRN,NETIPRD          FUDGE NETBLOCK FOR PRD2                
         MVC   NBPRD,NETIPRD                                                    
         MVC   NETIFILM,NETIFLM2        SET 2ND FILM CODE                       
*                                                                               
         MVC   NETIPRDC,NETIP2C         AND SET ALPHAS                          
         MVC   NBSPLPR3,NETIPRDC                                                
         MVC   NBPR1CL3,NETIPRDC                                                
*                                                                               
INV59C   MVI   MYBYTE,0                  CLEAR SPLIT FLAG                       
         B     INV60                     AND PASS TO DRIVER                     
*                                                                               
INV60    MVI   GLMODE,GLINPUT                                                   
         MVI   NBRDBELS,2         TELL NEWRIDRIVE TO SKIP ACCGEN                
         BAS   RE,SETNTBLK        SET NETBLOCK FOR NEWRIDRIVE                   
         MVC   NETIFLG,=C'INV'    SET TABLE ACTIVE                              
         MVC   SVINVKEY,KEY       SAVE OFF KEY                                  
*                                                                               
         GOTO1 NDDRIVER,DMCB,(R6)                                               
*                                                                               
         CLC   SVINVKEY,KEY       DID KEY CHANGE FROM DRIVER CALL?              
         BE    INV65              NO                                            
         MVC   KEY(32),SVINVKEY   YES - RESTORE KEY                             
         MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH               READ HIGH                                     
         LA    R2,KEY                                                           
         USING SNVKEYD,R2                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL  ',SNVDDA,MYIO,MYDM            
         DROP  R2                                                               
*                                                                               
INV65    CLI   MYBYTE,0            ARE WE SPLITTING COST?                       
         BNE   INV55               YES                                          
         B     INV70               NO                                           
*                                                                               
INV70    DS    0H                                                               
         B     INV2                   GET NEXT ELEM                             
*                                                                               
INVX     B     EXIT                                                             
*                                                                               
CHKFLTR  NTR1                                                                   
         CLC   NBSELPRD,=C'ALL'    PRODUCT FILTER ?                             
         BE    CKF10               NO                                           
         CLC   NBSELPRD,=C'POL'                                                 
         BE    CKF10               NO                                           
                                                                                
         CLI   NETIPRD,0           IS THERE A PRODUCT ON INVOICE ?              
         BNE   CKF05               YES                                          
         CLI   NETIPRDC,0          ALPHA PROD                                   
         BE    CKFNO               REJECT INV IF REQUEST FOR                    
*                                  A SPECIFIC PROD                              
                                                                                
* PRODUCT ON INVOICE - DOES IT MATCH FILTER?                                    
CKF05    CLI   NETIPRD,0            BINARY PROD ?                               
         BE    CKF05C                                                           
         CLI   NETIPRDC,0          YES,IF ALPHA AROUND ,USE THAT                
         BNE   CKF05C                                                           
         CLC   NETIPRD,NBEFFPNM    UES BINARY                                   
         BE    CKF10               MATCH                                        
         CLI   NETIPRD2,0          IS THERE 2ND PROD                            
         BE    CKFNO               NO - REJECT                                  
         CLC   NETIPRD2,NBEFFPNM   YES-TEST                                     
         BE    CKF10                                                            
         B     CKFNO                                                            
*                                                                               
CKF05C   DS    0H                                                               
         CLC   NETIPRDC,NBEFFPN3    ALPHA MATCH ?                               
         BE    CKF10                                                            
         CLI   NETIP2C,0           MATCH ON 2ND PRODUCT                         
         BE    CKFNO                                                            
         CLC   NETIP2C,NBEFFPN3     MATCH ON ALPHA 2ND PROD?                    
         BNE   CKFX                                                             
         BE    CKF10                                                            
*                                                                               
CKF10    DS    0H                                                               
         CLI   NETIEST,0           HAVE AN EST ON INV HEADER/DETAIL?            
         BNE   CKF20               YES                                          
***      CLI   NBSELEST,X'40'      ** TEST FOR X'40' IS WRONG **                
***      BH    CKFNO               ** TEST FOR X'40' IS WRONG **                
         CLI   NBSELEST,0          IS REQUEST IS FOR SPECIFIC ESTIMATE?         
         BH    CKFNO               YES - REJECT                                 
         OC    NBSELEFL,NBSELEFL   REQUESTING WITH ESTIMATE FILTERS?            
         BNZ   CKFNO               YES - REJECT                                 
         B     CKFYES              THIS ESTIMATE IS OK TO PROCESS               
*                                                                               
CKF20    CLI   NBSELEST,0          SINGLE ESTIMATE REQUEST?                     
         BE    CKF23               NO - ESTIMATE NO/ALL REQUEST                 
         CLI   NBSELESE,0          ESTIMATE RANGE?                              
         BE    CKF22               NO                                           
         CLC   NETIEST,NBSELESE    YES - EST > EST RANGE?                       
         BH    CKFNO               YES - REJECT                                 
         CLC   NETIEST,NBSELEST    EST < EST RANGE?                             
         BL    CKFNO               YES - REJECT                                 
         B     CKF23               TEST ESTIMATE FILTERS                        
*                                                                               
CKF22    CLC   NBSELEST,NETIEST    SPECIFIC ESTIMATE MATCH?                     
         BNE   CKFNO               NO - REJECT                                  
*                                                                               
CKF23    LA    R2,ESTMSK           LIST OF ESTIMATES TO PROCESS                 
         LLC   R1,NETIEST          EST ON INVOICE HEADER/DETAIL                 
         BAS   RE,TESTMASK         OK TO PROCESS THIS ESTIMATE?                 
         BE    CKFNO               NO - REJECT THIS EST                         
*                                                                               
CKFYES   SR    RE,RE                                                            
*                                                                               
CKFNO    LTR    RE,RE                                                           
*                                                                               
CKFX     B     EXIT                                                             
*                                                                               
* - FILL IN NETBLOCK TO FUDGE CLI/PROD/EST/MEDIA DRIVER ROUTINES                
*                                                                               
SETNTBLK NTR1                                                                   
         MVC   NBSELAM,NETIAM                   AGY/MED                         
         MVC   NBACTAM(3),NETIAM                AGY/MED+CLI CODE                
         L     R3,ANETWS1                                                       
         USING CLTHDR,R3                                                        
         GOTO1 NBCLUNPK,DMCB,(CPROF+6,NETICLT),NBCLICOD   CLIENT                
         DROP  R3                                                               
         MVC   NBACTEST,NETIEST                 EST                             
         MVC   NBACTNET,NETISTA                 STATION                         
         MVC   NBSPLPRN,NETIPRD                 PRODUCT (IF SPLITTING)          
         MVC   NBPRD,NETIPRD                                                    
         MVC   NBPRD2,NETIPRD2                                                  
         MVC   NBACTDAT,INDATSV                                                 
*                                                                               
         MVC   NBSPLPR3,NETIPRDC                                                
*                                                                               
         MVI   NBPOSTYP,C'N'       DEFAULT TO NETWORK FOR NOW                   
***      MVI   NBSTATYP,C'N'                                                    
         MVC   NBSTATYP,NBEFFMED   NBEFFMED FILLED IN ABOVE                     
**8      LA    R4,MYWORK                                                        
**       USING STAREC,R4                                                        
**       CLC   STAKCALL(4),NBACTNET    HAVE WE ALREADY DONE THIS?               
**       BE    SETNTBLX                                                         
**       NETGO NVSETSTA,DMCB                                                    
**       MVC   FILENAME,=C'STATION '                                            
**       MVI   USEIO,C'Y''                                                      
**       MVI   STAKTYPE,C'S'                                                    
**       MVI   STAKMED,C'N'                                                     
**       MVC   STAKCALL,NBACTNET                                                
**       MVI   STAKCALL+4,C'N'                                                  
**       MVC   STAKAGY,NBSELAGY                                                 
**       MVC   STAKCLT,=C'000'                                                  
**       GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'STATION',MYWORK,AIO,0               
**       CLC   KEY(9),KEYSAVE                                                   
**       BNE   FILNNEXT                                                         
**       MVC   NBSTATYP,STYPE      PICK OUT STATION TYPE                        
**       MVC   NBPOSTYP,SPTYPE     PICK OUT POSTING TYPE                        
**       NETGO NVSETUNT,DMCB                                                    
**       MVC   FILENAME,=C'XSPDIR  '                                            
**       MVI   USEIO,0                                                          
SETNTBLX B     EXIT                                                             
*                                                                               
* CALLED FROM BILLING RECORD READ TO RESTORE CLIENT RECORD                      
* TO ANETWS1                                                                    
RESTCLT  NTR1                                                                   
         L     R1,ANETWS1                                                       
         CLC   CLTRECSV,0(R1)                                                   
         BE    RCLTX                                                            
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEY,CLTRECSV                                                     
         GOTO1 HIGH                                                             
         MVC   AIO,ANETWS1                                                      
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
RCLTX    B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
* - READS  NEW CLIENT REC INTO ANETWS1 IF CLIENT HAS CHANGED                    
* - KEY = INVOICE REC KEY                                                       
CHKCLT   NTR1                                                                   
         MVI   BYTE,0                                                           
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLI   KEY,X'0B'           OLD INVOICE REC?                             
         BNE   CHKCLT05                                                         
         CLC   CKEYCLT,KEY+5       DO WE NEED NEW CLIENT HEADER                 
         BE    CHKCLTX             NO                                           
         BNE   CHKCLT10            YES                                          
                                                                                
CHKCLT05 CLC   =X'0E03',KEY        NEW INVOICE REC?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CKEYCLT,KEY+3       DO WE NEED NEW CLIENT HEADER                 
         BE    CHKCLTX             NO                                           
         BNE   CHKCLT10            YES                                          
                                                                                
CHKCLT10 MVC   SVINVKEY,KEY        YES/SAVE KEY                                 
         XC    KEY,KEY                                                          
         CLC   =X'0E03',SVINVKEY   NEW INVOICE KEY?                             
         BNE   *+14                                                             
         MVC   KEY+1(3),SVINVKEY+2 YES-AGY/MED + CLIENT                         
         B     CHKCLT15                                                         
         MVC   KEY+1(1),SVINVKEY+1 NO - MEDIA                                   
         MVC   KEY+2(2),SVINVKEY+5 CLIENT                                       
                                                                                
CHKCLT15 NETGO NVSETSPT,DMCB       SET FOR SPOT SYSTEM                          
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
***      BE    *+6               INVOICE RECORDS ARE NOT CLOSED OUT             
***      DC    H'0'              CAN HAVE CLIENT ON INVOICE BUT                 
         BNE   CLTNO             CLOSED OUT OTHERWISE                           
         MVC   FULL,AIO            SAVE I/O AREA                                
         MVC   AIO,ANETWS1         PUT CLIENT REC TO ANETWS1                    
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         MVC   NBEFFOFF,COFFICE    SET OFFICE                                   
         MVI   BYTE,C'Y'           SET NEW CLIENT HEADER FLAG                   
         MVC   AIO,FULL            RESET I/O AREA                               
         MVC   KEY(32),SVINVKEY    RESET KEY                                    
         MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH                AND SEQ READ                                 
         SR    RE,RE                                                            
         LTR   RE,RE                                                            
CHKCLTX  B     EXIT                                                             
* RETURN CC=NOT =                                                               
CLTNO    MVC   KEY(32),SVINVKEY    RESET KEY                                    
         MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH                AND SEQ READ                                 
         LTR   RE,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
* - EXPECTS R2 TO POINT TO EST/PRDMSK                                           
TESTMASK NTR1                                                                   
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         B     EXIT                                                             
                                                                                
SETMASK  NTR1                                                                   
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0               R1 HAS NUMBER                                
         SLDL  R0,29               GET BYTE NO. INTO R0                         
         SRL   R1,29                   BIT  NO. INTO R1                         
         AR    R2,R0               R2 HAS (MASK)                                
         LA    R1,BITLIST(R1)                                                   
         OC    0(1,R2),0(R1)                                                    
         B     EXIT                                                             
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
BITTEST  DS    CL1                                                              
HOLDEFLT DS    CL1                                                              
FFS      DC    32X'FF'                                                          
ESTMSK   DS    CL32                                                             
         EJECT                                                                  
*                                                                               
* - CLIENT GROUP FILTERING                                                      
*                                                                               
CGRPFILT NTR1                                                                   
         BAS   RE,CHKCLT           GETS NEW CLIENT REC IF NECESSARY             
         L     R4,ANETWS1                                                       
         USING CLTHDR,R4                                                        
VCL2     LA    R0,5                                                             
         LA    RF,CGRP1                                                         
         CLC   NBSELCGR(1),0(RF)       CHECK SCHEME LETTER                      
         BE    VCL5                                                             
VCL4     LA    RF,3(RF)                                                         
         BCT   R0,*-14                                                          
         B     VCLNO                                                            
VCL5     UNPK  DUB(5),1(3,RF)      UNPK PWOS                                    
         LA    R3,DUB                                                           
         LA    RE,NBSELCGR+1                                                    
         LA    R1,4                                                             
VCL6     CLI   0(RE),X'C1'         IF LETTER OR NUMBER                          
         BL    VCL7                                                             
         CLC   0(1,RE),0(R3)       MUST MATCH                                   
         BNE   VCL4                IF NO MATCH,TEST AGAINST NXT CGRP            
VCL7     LA    RE,1(RE)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,VCL6                                                          
         MVC   NBACTCGR(2),1(RF)   SET CLIENT GROUP CODE  PWOS                  
*                                                                               
VCLYES   SR    RE,RE               CLIENT PASSED TESTS                          
VCLNO    LTR   RE,RE                                                            
VCLX     B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* INPUT : WORK HAS STATION                                                      
*         NBNETGRP+1  HAS 500 STATIONS X 4                                      
*         NBNETGRP(0) =C'-' MEANS NEGATIVE FILTER                               
*                                                                               
SGRPFILT NTR1                                                                   
         L     R1,NBNETGRP                                                      
         CLI   0(R1),C'-'                                                       
         BNE   *+8                                                              
         MVI   BYTE,C'-'                                                        
         LA    R1,1(R1)                                                         
         LA    R2,500                                                           
SGRP10   CLI   0(R1),0                                                          
         BE    SGRP15                                                           
         CLC   WORK(4),0(R1)                                                    
         BE    SGRP25                                                           
         LA    R1,4(R1)                                                         
         BCT   R2,SGRP10                                                        
* NO MATCH                                                                      
SGRP15   CLI   BYTE,C'-'                                                        
         BE    SGRPYES                                                          
         B     SGRPNO                                                           
* STATION MATCH                                                                 
SGRP25   CLI   BYTE,C'-'           NEGATIVE FILTERING                           
         BE    SGRPNO                                                           
         B     SGRPYES                                                          
SGRPYES  SR    R1,R1                                                            
SGRPNO   LTR   R1,R1                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* - CLIENT OFFICE FILTERING                                                     
*   KEY = INVOICE RECORD                                                        
COFFFILT NTR1                                                                   
         BAS   RE,CHKCLT           GETS NEW CLIENT REC IF NECESSARY             
         BNE   OFFNO                                                            
                                                                                
         L     R4,ANETWS1                                                       
         USING CLTHDR,R4                                                        
                                                                                
         CLI   NBSELOFF,0          IF OFFICE FILTERING                          
         BE    COFOK                                                            
         TM    NBOFFTYP,X'80'      CHECK OFFICE LIST                            
         BO    OFFLIST                                                          
         TM    NBOFFTYP,X'40'      CHECK NEGATIVE FILTER                        
         BO    NEGOFF                                                           
         CLC   COFFICE,NBSELOFF                                                 
         BNE   OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
OFFLIST  OC    NBSECSV,NBSECSV         NEW SECURITY?                            
         BZ    OFLST10                                                          
         OC    NBTWAACC,NBTWAACC       AND TWA PASSED                           
         BNZ   NEWSEC                                                           
OFLST10  XC    DMCB(8),DMCB                                                     
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB(8),DUB                                                       
**       MVI   OFCSYS,C'N'                                                      
         MVI   OFCSYS,C'S'                                                      
         MVI   OFCAUTH,C'$'                                                     
         MVC   OFCAUTH+1(1),NBSELOFF                                            
         OI    OFCAUTH+1,X'C0'                                                  
         MVC   OFCAGY,NBEFFAGY                                                  
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
         SPACE 1                                                                
         GOTO1 AOFFICER,DMCB,DUB,NBACOM                                         
         TM    NBOFFTYP,X'40'      MAY BE NEGATIVE LIST                         
         BO    NEGLIST                                                          
         CLI   0(R1),0             IS OFFICE IN LIST                            
         BNE   OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
NEGLIST  CLI   0(R1),0             CHECK FOR NEGATIVE LIST                      
         BE    OFFNO                                                            
         B     COFOK                                                            
         SPACE 1                                                                
NEGOFF   CLC   NBEFFOFF,NBSELOFF                                                
         BE    OFFNO                                                            
*                                                                               
COFOK    DS    0H                                                               
*                                                                               
OFFYES   SR    RE,RE               CLIENT PASSED TESTS                          
OFFNO    LTR   RE,RE                                                            
OFFX     B     EXIT                                                             
*                                                                               
NEWSEC   DS    0H                                                               
         USING NEWSECD,R3                                                       
         ICM   R3,15,NBSECSV                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R2,MYWORK                                                        
         XC    MYWORK,MYWORK                                                    
         USING OFFICED,R2                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,NBTWAACC                                                 
         CLI   NBSELOFF,0              FILTERING ON OFFICE?                     
         BE    *+10                    NO                                       
         MVC   OFCAUTH+1(1),NBSELOFF   YES-SET IT                               
         MVC   OFCAGY,NBEFFAGY                                                  
         MVC   OFCOFC,NBEFFOFF                                                  
         GOTO1 NBCLUNPK,DMCB,(CPROF+6,NBACTCLI),OFCCLT                          
         OC    OFCCLT,=X'404040'                                                
         MVC   OFCSAGMD,NBACTAM                                                 
         MVC   OFCLMT(4),NBTWAACC                                               
         CLI   NBSELOFF,0              FILTERING ON OFFICE?                     
         BE    *+10                                                             
         MVC   OFCLMT+1(1),NBSELOFF    YES-SET IT                               
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         LA    RF,NEWSECA                                                       
         ST    RF,OFCSECD                                                       
*                                                                               
         XC    DMCB(16),DMCB                                                    
         GOTO1 AOFFICER,DMCB,(C'N',MYWORK),NBACOM                               
*                                                                               
         DROP  R2                                                               
*                                                                               
         TM    NBOFFTYP,X'40'      MAY BE NEGATIVE LIST                         
         BO    NEGLIST2                                                         
         CLI   0(R1),0             IS OFFICE IN LIST                            
         BNE   OFFNO                                                            
***      CLI   BYTE,0              NEED TO RESTORE?                             
***      BE    *+10                                                             
***      MVC   NBSELOFF,BYTE       YES                                          
         B     OFFYES                                                           
         SPACE 1                                                                
NEGLIST2 CLI   0(R1),0             CHECK FOR NEGATIVE LIST                      
         BE    OFFNO                                                            
***      CLI   BYTE,0              NEED TO RESTORE?                             
***      BE    *+10                                                             
***      MVC   NBSELOFF,BYTE       YES                                          
         B     OFFYES                                                           
         SPACE 1                                                                
         DROP  R4                                                               
***                                                                             
* CALLED WHEN CLIENT CHANGES RESETS ESTIMATE MASK FOR NEW CLIENT                
***                                                                             
SETEST   NTR1                                                                   
*                                                                               
         MVC   SVINVKEY,KEY        SAVE INVOICE KEY                             
         XC    ESTMSK,ESTMSK       CLEAR ESTMSK                                 
         MVI   ESTFLAG,C'N'        DIDN'T DO A GETREC YET                       
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R4,KEY              R4 = KEY                                     
         USING ESTHDR,R4           ESTIMATE DSECT                               
         L     R1,ANETWS1          A(CLIENT RECORD)                             
         MVC   EKEYAM(3),1(R1)     SET A/M AND CLIENT IN KEY                    
         MVC   EKEYPRD,NBSELPRD    PRODUCT                                      
         CLC   EKEYPRD,=C'ALL'     NBSELPRD=ALL?                                
         BNE   *+10                NO                                           
         MVC   EKEYPRD,=C'POL'     YES - READ POL PRODUCT                       
         MVC   EKEYEST,NBSELEST    ESTIMATE                                     
*                                                                               
         NETGO NVSETSPT,DMCB       SET FOR SPOT SYSTEM                          
         MVC   FILENAME,=C'SPTDIR  '                                            
*                                                                               
         CLI   NBSELESE,0          EST RANGE?                                   
         BNE   SETE5               YES                                          
         CLI   NBSELEST,0          SINGLE ESTIMATE?                             
         BNE   SETE5               YES                                          
*                                                                               
         MVI   EKEYEST,1           ALL ESTIMATE REQUEST                         
         GOTO1 HIGH                                                             
         CLI   EKEYEST+1,0         HAVE AN ESTIMATE RECORD?                     
         B     SETE10              GO TEST CC                                   
*                                                                               
SETE5    GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      FOUND ANY ESTIMATE?                          
SETE10   BNE   SETE50              NO - JUST EXIT                               
*                                                                               
         MVC   ESTMSK,FFS          FOR NO/ALL USE FF'S                          
         CLI   NBSELEST,0          SINGLE EST/EST RANGE?                        
         BE    SETE20              NO - NO/ALL ESTIMATE REQUEST                 
*                                                                               
         XC    ESTMSK,ESTMSK       YES - CLEAR ESTMSK                           
         LLC   R1,NBSELEST         ESTIMATE                                     
         XR    R3,R3               CLEAR R3                                     
         ICM   R3,1,NBSELESE       EST RANGE?                                   
         BNZ   *+6                 YES                                          
         LR    R3,R1               NO - SET EST END TO EST START                
         LA    R2,ESTMSK           ESTIMATE MASK                                
SETE15   BAS   RE,SETMASK          FOR SINGLE ESTIMATE                          
         LA    R1,1(R1)            BUMP TO NEXT ESTIMATE                        
         CR    R1,R3               MORE ESTIMATES TO PROCESS                    
         BNH   SETE15              YES                                          
         B     SETE50              DONE                                         
*                                                                               
SETE20   OC    NBSELEFL,NBSELEFL   HAVE ESTIMATE FILTERS?                       
         BZ    SETE50              NO - DONE                                    
*                                                                               
         XC    ESTMSK,ESTMSK       CLEAR ESTIMATE MASK                          
         MVI   ESTFLAG,C'Y'        INDICATE THAT WE DID A GETREC                
         MVC   AIO,ANETWS1         READ ESTIMATE REC INTO ANETWS1               
         B     SETE30              ALREADY GOT THE FIRST ESTIMATE               
*                                                                               
SETE25   LA    R4,KEY              RE-POINT R4 TO KEY                           
         CLI   EKEYEST,255         JUST PROCESSED LAST POSSIBLE EST?            
         BE    SETE50              YES - DONE                                   
         AI    EKEYEST,1           SKIP TO NEXT ESTIMATE                        
         LA    R2,EKEYEST+1        POINT PAST ESTIMATE                          
         XC    0(5,R2),0(R2)       CLEAR IN CASE WE HAVE BILLING REC            
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      AGY/MED/CLT/PRD MATCH?                       
         BNE   SETE50              NO - DONE PROCESSING ESTIMATES               
         OC    0(5,R2),0(R2)       HAVE BILLING REC?                            
         BNZ   SETE25              YES - GET NEXT ESTIMATE                      
*                                                                               
SETE30   MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R4,AIO              A(ESTIMATE RECORD)                           
         LA    R3,NBSELEFL         ESTIMATE FILTERS                             
         LA    R5,EPROF            FILTERS ON ESTIMATE RECORD                   
         LA    R0,3                3 FILTERS                                    
*                                                                               
SETE35   CLI   0(R3),C'*'          USER REQUESTED WILDCARD?                     
         BE    SETE45              YES - EVERYTHING MATCHES THIS                
         CLI   0(R3),0             HAVE REQ FILTER IN THIS POSITION?            
         BE    SETE45              NO - BUMP TO NEXT FILTER POSITION            
         TM    0(R3),X'40'         NEGATIVE FILTER?                             
         BZ    SETE40              YES                                          
         CLC   0(1,R3),0(R5)       FILTERS MATCH?                               
         BNE   SETE25              NO - REJECT ESTIMATE                         
         B     SETE45              YES - PROCESS NEXT FILTER                    
*                                                                               
SETE40   MVC   HOLDEFLT,0(R5)      HOLD EPROF FILTER                            
         NI    HOLDEFLT,X'FF'-X'40' TURN OFF X'40' BIT                          
         CLC   0(1,R3),HOLDEFLT    NEGATIVE FILTER MATCHES?                     
         BE    SETE25              YES - REJECT ESTIMATE                        
*                                                                               
SETE45   LA    R3,1(R3)            BUMP TO NEXT NBSELEFL POSITION               
         LA    R5,1(R5)            BUMP TO NEXT EPROF POSITION                  
         BCT   R0,SETE35           PROCESS FILTER                               
*                                                                               
         LLC   R1,EKEYEST          ESTIMATE PASSED FILTERS                      
         LA    R2,ESTMSK           ESTIMATE MASK                                
         BAS   RE,SETMASK          UPDATE ESTMSK                                
         B     SETE25              READ NEXT ESTIMATE                           
         DROP  R4                  DROP ESTIMATE REC USING                      
*                                                                               
SETE50   CLI   ESTFLAG,C'Y'        DID WE DO A GETREC?                          
         BNE   SETE60              N0                                           
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R1,SVINVKEY         A(INVOICE KEY)                               
         MVC   KEY+1(3),2(R1)      SET A/M AND CLIENT IN KEY                    
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
*                                                                               
SETE60   MVC   KEY(32),SVINVKEY    RESET KEY                                    
         MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH                AND SEQ READ                                 
*                                                                               
         B     EXIT                                                             
*                                                                               
* - FILL IN PRODMSK FOR PRODUCT GROUP FILTER                                    
SETPRD   NTR1                                                                   
         MVC   AIO,NBAIO                                                        
         LA    R4,KEY              NO PRODUCT NUMBER GIVEN                      
         XC    KEY,KEY                                                          
         USING PRDHDR,R4                                                        
         L     R1,ANETWS1          CLIENT RECORD                                
         MVC   PKEYAM(3),1(R1)        SET A/M AND CLIENT IN KEY                 
         CLC   =C'ALL',NBSELPRD                                                 
         BE    SETPGRP                                                          
         MVC   PKEYPRD,NBSELPRD                                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         B     SETPX                                                            
*                                                                               
SETPGRP  DS    0H                                                               
         CLI   NBSELPGR,0          IF NO PRODUCT GROUP                          
         BNE   QI21                                                             
****     MVC   NBPRDMSK,FFS        SET PRDMSK AND EXIT                          
         B     EXIT                                                             
         SPACE 1                                                                
QI21     LA    R4,KEY                                                           
         XC    PKEYPRD,PKEYPRD                                                  
         SPACE 1                                                                
QI22     LA    R4,KEY              GET NEXT PRODUCT                             
         AI    PKEYPRD+2,1                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    QI23                                                             
         MVC   KEY,KEYSAVE         NEED TO RESORE POL                           
         CLC   NBSELPRD,=C'POL'                                                 
         BNE   SETPX                                                            
         MVC   PKEYPRD,=C'POL'                                                  
         GOTO1 HIGH                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         B     SETPX                                                            
         SPACE 1                                                                
QI23     MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         LA    R2,PGRP1            SELECT ASSIGNMENT (V,W,X)                    
         CLC   NBSELPGR(1),PGRP1                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23A                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP1                                                       
         BE    QI22                                                             
QI23A    LA    R2,PGRP2                                                         
         CLC   NBSELPGR(1),PGRP2                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23B                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP2                                                       
         BE    QI22                                                             
QI23B    LA    R2,PGRP3                                                         
         CLC   NBSELPGR(1),PGRP3                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23C                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP3                                                       
         BE    QI22                                                             
QI23C    LA    R2,PGRP4                                                         
         CLC   NBSELPGR(1),PGRP4                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BNO   QI23D                                                            
         MVC   BYTE,NBSELPGR                                                    
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP4                                                       
         BE    QI22                                                             
QI23D    LA    R2,PGRP5                                                         
         CLC   NBSELPGR(1),PGRP5                                                
         BE    QI24                                                             
         TM    NBSELPGR,X'40'      IS IT NEGATIVE FILTER                        
         BO    QI22                NO / GET NEXT REC                            
         MVC   BYTE,NBSELPGR       YES/CHECK IT                                 
         OI    BYTE,X'40'                                                       
         CLC   BYTE,PGRP5                                                       
         BE    QI22                                                             
         SPACE 1                                                                
QI24     UNPK  DUB(5),1(3,R2)      SEE IF THIS PRODUCT IS IN                    
         LA    RF,DUB                                                           
         LA    RE,NBSELPGR+1                                                    
         LA    R0,4                                                             
         SPACE 1                                                                
QI25     CLI   0(RE),C'*'          WILD CARD OK                                 
         BE    QI26                                                             
         CLI   0(RE),X'40'         BLANKS/ZERO OK                               
         BNH   QI26                                                             
         CLC   0(1,RE),0(RF)       ELSE IT MUST MATCH                           
         BE    QI26                                                             
         TM    0(RE),X'40'         IS IT NEGATIVE FILTER                        
         BO    QI22                NO                                           
         MVC   BYTE,0(RE)          YES/CHECK IT                                 
         OI    BYTE,X'40'                                                       
         CLC   BYTE,0(RF)                                                       
         BE    QI22                                                             
         SPACE 1                                                                
QI26     LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,QI25                                                          
         SPACE 1                                                                
         LH    R1,PCODE            PASSED THE TESTS SO...                       
***      BAS   RE,FILLPDET         FILL IN DETAILS                              
***      LA    R2,NBPRDMSK                                                      
***      BAS   RE,SETMASK          SET THIS PRODUCT AS OK                       
*****    BAS   RE,SETOPMSK         IT APPEARS WE NEVER TEST EST OR PRDS         
         B     QI22                                                             
SETPX    B     EXIT                                                             
         SPACE 1                                                                
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
*                                                                               
GETPRD3  NTR1                  RETURNS 3 BYTE PROD IN WORK                      
         L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         MVI   BYTE,0                                                           
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GP5      CLC   WORK(1),3(R1)                                                    
         BE    GOTPRD3                                                          
         LA    R1,4(R1)                                                         
         BCT   R2,GP5                                                           
         CLI   BYTE,1                                                           
         BE    GP8                                                              
         MVI   BYTE,1                                                           
         L     R1,ANETWS1                                                       
         LA    R1,CLIST2                                                        
         LA    R2,35                                                            
         B     GP5                                                              
*                                                                               
***GP8      BAS   RE,GETPRD30    WHAT WAS I THINKING?                           
***                              ONLY WRITER CALL THIS AND CLIENT               
***                              SITS IN ANETWS1                                
GP8      MVC   WORK(3),=C'***'                                                  
         B     *+10                                                             
GOTPRD3  MVC   WORK(3),0(R1)                                                    
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*******************************************************                         
*          DATA SET NENETVALUS AT LEVEL 107 AS OF 03/21/06                      
*****************************************************                           
* INPUT - BYTE = 1 BYTE PROD CODE                                               
* OUTPUT- WORK = 3 CHAR PROD                                                    
GETPRD30 NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PLSTPSSV,R4                                                      
         MVC   WORK(2),=X'0DF1'                                                 
         MVC   WORK+2(3),NBACTAM                                                
         MVC   PLSTBPRD+1(1),BYTE                                               
         MVC   WORK+20(13),WORK                                                 
         GOTO1 NBDM,DMCB,=C'DMRDHI',=C'SPTDIR',WORK,WORK                        
         B     GETPRD39                                                         
*                                                                               
GTPRSEQ0 GOTO1 NBDM,DMCB,=C'DMRSEQ',=C'SPTDIR',WORK,WORK                        
*                                                                               
GETPRD39 CLC   WORK+20(5),WORK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PLSTBPRD+1(1),BYTE                                               
         BNE   GTPRSEQ0                                                         
         MVC   WORK(3),PLSTPRD                                                  
         XIT1                                                                   
         DROP  R4                                                               
*******************************************************                         
*                                                                               
*GETPRD1  NTR1                                                                  
*         L     R1,ANETWS1      1 BYTE PROD IN WORK                             
*         USING CLTHDR,R1                                                       
*         LA    R1,CLIST                                                        
*         LA    R2,220                                                          
* GTP5     CLC   BHACTPRD,0(R1)                                                 
*         BE    GOTPRD                                                          
*         LA    R1,4(R1)                                                        
*         BCT   R2,GTP5                                                         
*         MVC   WORK(1),0                                                       
*         B     *+10                                                            
*GOTPRD   MVC   WORK(1),3(R1)                                                   
*         B     EXIT                                                            
         EJECT                                                                  
*          DATA SET NEWRI20    AT LEVEL 022 AS OF 03/17/95                      
HOOK     NTR1                                                                   
         CLI   GLHOOK,GLHEAD                                                    
         BNE   HOOK2                                                            
         GOTO1 NDGENHED                                                         
         B     EXIT                                                             
         SPACE 1                                                                
HOOK2    CLI   GLHOOK,GLPRINT                                                   
         BNE   EXIT                                                             
         TM    NDLININD,X'80'                                                   
         BNO   *+8                                                              
         MVI   GLHOOK,GLDONT                                                    
         MVI   NDLININD,0          RESET LINE INDICATORS                        
         B     EXIT                                                             
*                                                                               
XFF      DC    10X'FF'                                                          
         EJECT                                                                  
         LTORG                                                                  
INVDATA  DS    CL(NETINVBE)                                                     
*                                                                               
         DS    0F                                                               
         DC    C'MYIO'                                                          
MYIO     DS    CL4000                                                           
PRDMSK   DS    CL600        (CL3 PROD) X 200                                    
*                                                                               
ELEM30S  DS    XL(200*SNVCMLNQ)      (20 BYTES X 200 ELEMS)                     
ELEM30E  EQU   *-ELEM30S                                                        
ELEMNUM  EQU   200                      HARDCODED !!!                           
         DC    X'FF'                                                            
*                                                                               
NOMINIO  EQU   SNVKMINK-SNVKEY       INVOICE KEY LENGTH W/O MINIO KEY           
*                                                                               
ELEMF1   DS    XL(ACTVLENQ)          SAVED ACTIVITY ELEMENT FROM INV            
*                                                                               
* - WORK AREA FOR RDBELEM                                                       
WRKAREA  DSECT                                                                  
*                                                                               
* NETWORK INVOICE BLOCK                                                         
*                                                                               
*                                                                               
MYDM     DS    CL96                                                             
ABELEM   DS    F                                                                
ADRIVHK  DS    F                  R5 SAVED FOR DRIVER IN CALLING MODULE         
AMYIO    DS    F                   ADDRESS PASSED BY CALLING MODULE             
ADR30S   DS    F                                                                
         DS    0D                                                               
MYDUB    DS    CL8                                                              
COMPLEN  DS    CL1                                                              
CLTSV    DS    CL2                                                              
STASV    DS    CL5                                                              
DATSV    DS    CL8                                                              
DUDATSV  DS    CL8                                                              
INDATSV  DS    CL2                                                              
IAMSV    DS    CL1                 AGENCY/MEDIA                                 
COSTSV   DS    CL4                                                              
COSTSV2  DS    PL8                                                              
PAKWRK   DS    PL9                                                              
MYKEY    DS    CL20                                                             
SVKEY    DS    CL20                                                             
MYKEYSV  DS    CL32                                                             
CLTRECSV DS    CL13                                                             
PROFILSV DS    CL10                                                             
NETINSV  DS    CL10                                                             
PERDATSV DS    CL6                                                              
NEWMINRC DS    CL1                                                              
INVBREAK DS    CL1                                                              
*                                                                               
INVRDCLI DS    CL2                 REQUEST CLIENT                               
INVRDSTA DS    CL3                 REQUEST STATION  (PACKED)                    
INVRDSDT DS    XL2                 REQUEST START DATE                           
INVRDEDT DS    XL2                 REQUEST END DATE                             
INVSTMOS DS    XL2                 START YYMM01 MOS                             
INVENMOS DS    XL2                 END   YYMMO1 MOS                             
*                                                                               
HEDEST   DS    CL1                 INVOICE HEADER ESTIMATE                      
HEDPRD   DS    CL1                 INVOICE HEADER PRODUCT                       
HEDPRD2  DS    CL1                 INVOICE HEADER PRODUCT2                      
HEDPRDA  DS    CL3                 INVOICE ALPHA PROD                           
HEDPRDA2 DS    CL3                 INVOICE ALPHA PROD 2                         
HEDINVNO DS    CL10                INVOICE HEADER INVOICE NO                    
HEDLENE  EQU   *-HEDEST                                                         
*                                                                               
MYWORK   DS    CL100                                                            
MYWORK2  DS    CL100                                                            
MYWORK3  DS    CL300                                                            
MYBYTE   DS    CL1                                                              
SVINVKEY DS    XL32                                                             
LASTCLT  DS    XL2                                                              
ESTFLAG  DS    CL1                                                              
WRKLEN   EQU   *-WRKAREA                                                        
*                                                                               
*                                                                               
* NEGENINCLS                                                                    
* NEDATELSTD                                                                    
* DRGLOBAL                                                                      
* SPGENPRD                                                                      
* NEGENCOM                                                                      
* NECOMBLOK                                                                     
* SPGENBILL                                                                     
* SPBVALD                                                                       
* BHBLOCKD                                                                      
* NEGBLOCKD                                                                     
* NEGENUNIT                                                                     
* NEGENPACK                                                                     
* SPGENCLT                                                                      
         EJECT                                                                  
       ++INCLUDE NEGENINCLN                                                     
       ++INCLUDE NEGENNBUFF                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE NENETINVD                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE NETSECD                                                        
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE0D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052NEWRI84   05/05/20'                                      
         END                                                                    
