*          DATA SET BUFIL44    AT LEVEL 089 AS OF 05/01/02                      
*PHASE T50244A                                                                  
         TITLE 'T50244 - BUDGET EXTRACT - PRINTPAK'                             
T50244   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         NMOD1 0,T50244,RA,RR=RE                                                
         ST    RE,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         MVI   DMINBTS,0           CLEAR DMGR BIT SETTING                       
         L     R0,VADUMMY                                                       
         ST    R0,APUBIO           FOR PUBIO NEED 4K                            
         AH    R0,=H'4000'                                                      
         ST    R0,ALTLIO           NEED FOR LITTLE PUBREC                       
         AH    R0,=H'4000'                                                      
         ST    R0,NEXTADDR         SET NEXT AVAILABLE CORE ADDRESS              
*                                                                               
         L     R3,ATWA                                                          
         USING T502FFD,R3                                                       
*                                                                               
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         MVC   BINSRCH,TBINSRCH    EXTRACT V(BINSRCH)                           
*                                                                               
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 TWAVBUFF,DMCB,=C'SET',BUFFBUFF                                   
*                                                                               
         LA    R1,SVDTYPES                                                      
         USING SVDTD,R1                                                         
         LA    R0,MAXDTYP                                                       
         XC    DATAINDS,DATAINDS   CLEAR EXTRACT TYPE INDICATORS                
         SR    R4,R4                                                            
         LA    R5,EXTYPS                                                        
*                                                                               
SP2      CLI   SVDTEX,0            TEST FOR EOT                                 
         BE    SP6                 YES                                          
         LA    RE,EXTTBL           RE=A(VALID DATA TYPE TABLE)                  
         LA    RF,EXTYPES          RF=N'VALID DATA TYPES                        
*                                                                               
SP3      CLC   SVDTEX,0(RE)        TEST IF VALID FOR PRINT EXTRACT              
         BE    SP4                 YES                                          
         LA    RE,L'EXTTBL(RE)                                                  
         BCT   RF,SP3                                                           
         B     SP5                                                              
*                                                                               
SP4      OC    DATAINDS,1(RE)      UPDATE CUMULATIVE MASK                       
         MVC   0(1,R5),0(RE)       ADD EXTRACT TYPE TO TABLE                    
         LA    R4,1(R4)            INCREMENT EXTRACT TYPE COUNT                 
         LA    R5,1(R5)                                                         
*                                                                               
SP5      LA    R1,SVDTL(R1)        NEXT DATA TYPE                               
         BCT   R0,SP2                                                           
         SPACE 2                                                                
SP6      LTR   R4,R4               TEST FOR ANYTHING TO EXTRACT                 
         BZ    EXIT                                                             
         STC   R4,NEXTYPS                                                       
         MVC   BUYRB,ORDEREDB                                                   
         OC    BUYRB,BILLEDB       FORM BUY RECORD DATA BIT MASK                
*                                                                               
SP8      L     R2,ARULDATA         GET ADDRESS OF FIRST RULE                    
         USING QRD,R2                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QRAGYC                                                    
         MVC   KEY+2(1),QRMED                                                   
         MVI   KEY+3,X'01'         MUST READ AGYHDR FOR PROFILE                 
         BAS   RE,SPHIGH                                                        
         CLC   KEY(4),KEYSAVE                                                   
         BNE   BADAGY                                                           
         SPACE 1                                                                
* READ AGENCY HEADER                                                            
         SPACE 1                                                                
         MVC   AIO,AIO3                                                         
         BAS   RE,SPGET                                                         
*                                                                               
         L     R6,AIO                                                           
         MVC   SVAPROF,PAGYPROF-PAGYREC(R6)                                     
*                               READ CLIENT HDR                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),QRAGYC                                                    
         MVC   KEY+2(1),QRMED                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),QRCLT                                                   
         BAS   RE,SPHIGH                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BNE   BADCLT                                                           
         SPACE 1                                                                
* READ CLIENT HEADER                                                            
         SPACE 1                                                                
         MVC   AIO,AIO3                                                         
         BAS   RE,SPGET                                                         
*                                                                               
         L     R6,AIO                                                           
         MVC   SVCLNAME,PCLTNAME-PCLTREC(R6)                                    
*                                                                               
SP10     MVC   PRD,QRPRD           SET ACTIVE PRODUCT                           
         MVI   ALLSW,YES           SET ALL PRODUCT EXTRACT                      
         CLC   PRD,=C'ALL'         TEST ALL PRODUCTS REQUESTED                  
         BE    SP12                YES                                          
         MVI   ALLSW,NO                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QRAGYC                                                    
         MVC   KEY+2(1),QRMED                                                   
         MVI   KEY+3,X'06'           VALIDATE PRODUCT                           
         MVC   KEY+4(3),QRCLT                                                   
         MVC   KEY+7(3),QRPRD                                                   
         BAS   RE,SPHIGH                                                        
         CLC   KEY(10),KEYSAVE                                                  
         BNE   BADPRD                                                           
         B     SP14                                                             
*                                                                               
SP12     XC    KEY,KEY             PRODUCT=ALL - READ FIRST PRODUCT             
         MVC   KEY(2),QRAGYC                                                    
         MVC   KEY+2(1),QRMED                                                   
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),QRCLT                                                   
SP13     BAS   RE,SPHIGH                                                        
         CLC   KEY(7),KEYSAVE      TEST SAME AGY/MED/CLIENT                     
         BNE   SP900               NO PRODUCTS FOR THE CLIENT                   
         MVC   PRD,KEY+7           EXTRACT FIRST PRODUCT CODE                   
         B     SP14                                                             
*                                                                               
SP14     MVC   AIO,AIO1                                                         
         BAS   RE,SPGET                                                         
*                                                                               
         L     R6,AIO                                                           
         USING PPRDRECD,R6                                                      
         MVC   DIV,PPRDDIV       SAVE THIS PRD'S DIVISION                       
*                                                                               
         DROP  R6                                                               
         MVI   PRDSW,C'N'                                                       
         L     R2,ARULDATA                                                      
SP17     MVI   QRPRDSW,C'Y'             DEFAULT TO YES                          
         OC    QRDIVDSP,QRDIVDSP       CHK FOR DIVISION LIST                    
         BNZ   SP18                     NO                                      
         MVI   PRDSW,C'Y'              THIS RULE HAS NO DIVS                    
         B     SP24                                                             
*                                                                               
SP18     MVI   QRPRDSW,C'N'                                                     
         SR    R7,R7                                                            
         ICM   R7,3,QRDIVDSP                                                    
         AR    R7,R2             POINT TO LIST OF PUB LISTS                     
         ZIC   R8,0(R7)          GET NUMBER OF PUB LISTS                        
         LA    R7,1(R7)          POINT TO FIRST ENTRY                           
SP19     CLC   DIV,0(R7)         SEE IF I MATCH DIVISIONS                       
         BNE   SP22              YES - SOME RULE USES THIS PRD                  
         MVI   QRPRDSW,C'Y'      THIS RULE USES THIS PRD                        
         MVI   PRDSW,C'Y'                                                       
         B     SP24                                                             
*                                                                               
SP22     LA    R7,3(R7)           NEXT DIV                                      
         BCT   R8,SP19                                                          
         CLC   QRPRD,=C'ALL'                                                    
         BNE   BADPRD                                                           
SP24     ICM   R2,15,QRNEXT        ADVANCE TO NEXT RULE                         
         BNZ   SP17                                                             
         CLI   PRDSW,C'Y'          SOME RULE USES THIS PRD                      
         BE    SP25                                                             
         MVI   KEY+10,X'FF'                                                     
         B     SP13                SKIP TO NEXT PRD                             
         EJECT                                                                  
* BUILD LIST OF ESTIMATES COVERING ANY PART OF PLAN PERIOD                      
* AND SAVE THE ESTIMATE FILTER VALUES                                           
         SPACE 1                                                                
SP25     L     R2,ARULDATA         RESET R2 TO FIRST RULE                       
         L     R0,AESTTAB                                                       
         L     R1,=F'4096'         CLEAR ESTTAB (4K)                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QRAGYC                                                    
         MVC   KEY+2(1),QRMED                                                   
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),QRCLT                                                   
         MVC   KEY+7(3),PRD                                                     
*                                                                               
SP32     BAS   RE,SPHIGH                                                        
         B     SP34D                                                            
*                                                                               
SP34     BAS   RE,SPSEQ                                                         
SP34D    CLC   KEY(10),KEYSAVE     FIND PRDHDR                                  
         BNE   SP40                NO EST FOUND FOR PRD                         
*                                                                               
         MVC   AIO,AIO1                                                         
         BAS   RE,SPGET                                                         
*                                                                               
         L     R6,AIO                                                           
         USING PESTRECD,R6                                                      
*                                                                               
         TM    DATAIND1,EXBAINVB                                                
         BNZ   SP35                                                             
*                                                                               
         MVC   HALF,DATAINDS                                                    
         NC    HALF,BUYRB          TEST EXTRACT FROM BUY RECORD                 
         BZ    *+12                NO                                           
         TM    PLANIND,BUPLNBIL    TEST BILLABLE DATE OPTION                    
         BO    SP35                YES-LOOK AT ALL ESTIMATES                    
*                                                                               
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BNE   SP34E               NO                                           
         TM    DATAIND,EXBDATB+EXBNDATB+EXBINVB+EXBNINVB                        
         BNZ   SP35                YES-SKIP ESTIMATE DATE TEST                  
         TM    DATAIND1,EXBADATB+EXBDDATB                                       
         BNZ   SP35                                                             
*                                                                               
SP34E    CLC   PESTST,SVEXTEND     EST START AFTER PLAN END                     
         BH    SP34                                                             
         CLC   PESTEND,SVEXTST     EST END BEFORE PLAN START                    
         BL    SP34                                                             
         SPACE 1                                                                
* ADD ESTIMATE TO TABLE                                                         
         SPACE 1                                                                
SP35     MVC   HALF(2),KEY+10                                                   
         LH    RE,HALF                                                          
         SLL   RE,2                X 4                                          
         A     RE,AESTTAB                                                       
         MVC   0(3,RE),PESTGRPS    SAVE FILTER VALUES                           
         OC    0(3,RE),0(RE)                                                    
         BNZ   *+10                                                             
         MVC   0(3,RE),=C'   '                                                  
         MVC   3(1,RE),PESTTEST                                                 
         B     SP34                                                             
         EJECT                                                                  
         SPACE 1                                                                
SP40     DS    0H                                                               
         L     R2,ARULDATA                                                      
SP41     OC    QRLSTDSP,QRLSTDSP      CHK FOR PUB LISTS                         
         BZ    SP48                                                             
         L     R5,NEXTADDR                                                      
         MVC   0(8,R5),=C'*PUBLST*'                                             
         ST    R8,8(R5)                                                         
         LA    R5,12(R5)                                                        
         STCM  R5,15,QRADRMGR                                                   
         SR    R7,R7                                                            
         ICM   R7,3,QRLSTDSP                                                    
         AR    R7,R2             POINT TO LIST OF PUB LISTS                     
         ZIC   R8,0(R7)          GET NUMBER OF PUB LISTS                        
         LA    R7,1(R7)          POINT TO FIRST ENTRY                           
*                                                                               
*        READ PUB LISTS AND CREATE LIST OF PUBS                                 
*                                                                               
SP42     XC    KEY,KEY                                                          
         MVC   KEY(2),QRAGYC                                                    
         MVC   KEY+2(1),QRMED                                                   
         MVI   KEY+3,X'17'                                                      
         MVC   KEY+4(3),QRCLT                                                   
         MVC   KEY+7(3),0(R7)       PUB LIST CODE                               
         BAS   RE,SPHIGH                                                        
         B     SP42D                                                            
*                                                                               
SP42C    BAS   RE,SPSEQ                                                         
SP42D    CLC   KEY(10),KEYSAVE                                                  
         BNE   SP46                NOT FOUND OR END OF LIST RECS                
         MVC   AIO,AIO3                                                         
         BAS   RE,SPGET                                                         
         MVI   ELCDLO,X'20'                                                     
         L     R6,AIO                                                           
         LA    R6,33(R6)                                                        
         CLC   0(1,R6),ELCDLO                                                   
         BE    SP44                                                             
SP43     BAS   RE,NEXTEL                                                        
         BNE   SP42C                                                            
*                                                                               
SP44     MVC   0(6,R5),2(R6)      PUT PUB IN LIST                               
         LA    R5,6(R5)                                                         
         B     SP43                                                             
*                                                                               
SP46     XC    0(6,R5),0(R5)      SET E-O-L FLAG                                
         LA    R7,3(R7)           NEXT PUB LIST                                 
         BCT   R8,SP42                                                          
         LA    R5,6(R5)                                                         
         ST    R5,NEXTADDR                                                      
         ICM   R2,15,QRNEXT        ADVANCE TO NEXT RULE                         
         BNZ   SP41                                                             
*                                                                               
SP48     DS    0H                                                               
SP50     DS    0H                                                               
         SPACE 1                                                                
* PROCESS BUY RECORDS *                                                         
         SPACE 1                                                                
SP60     MVC   HALF,DATAINDS                                                    
         NC    HALF,BUYRB    TEST EXTRACT ORDERED,OR BILLED,OR PAID             
         BZ    SP100         NO SKIP BUYREC PROCESSING                          
*                                                                               
         TM    DATAIND,EXBINVB+EXBNINVB TEST BILL BY INVOICE DATE               
         BZ    *+8                                                              
         BAS   RE,BLDINV           YES-BUILD INVOICE BUFFER FOR PRODUCT         
*                                                                               
         XC    LASTEST,LASTEST                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),QRAGYC       AGY                                          
         MVC   KEY+2(1),QRMED                                                   
         MVI   KEY+3,X'20'                                                      
         MVC   KEY+4(3),QRCLT                                                   
         MVC   KEY+7(3),PRD                                                     
*                                                                               
SP61     OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,SPHIGH                                                        
         B     SP64                                                             
*                                                                               
SP62     DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,SPSEQ                                                         
*                                                                               
SP64     CLC   KEY(10),KEYSAVE     SAME A-M/C/P                                 
         BNE   SP72                                                             
*                                                                               
         CLI   KEY+L'PBUYKEY,X'FF' TEST FOR RE-USED POINTER                     
         BE    SP62                YES-SKIP THE POINTER                         
*                                                                               
         CLI   ALLSW,YES           TEST PRODUCT=ALL EXTRACT                     
         BNE   *+14                                                             
         OC    KEY+21(3),KEY+21    TEST FOR PASSIVE POINTER                     
         BNZ   SP62                YES-SKIP RECORD                              
*                                                                               
         CLC   KEY+10(6),KEYSAVE+10      SAME PUB                               
         BE    *+8                                                              
         BAS   RE,FINDPUB          GO READ NEW PUB RECORD                       
*                                                                               
         CLC   KEY(16),KEYSAVE     SAME A-M/C/P/PUB                             
         BNE   SP65                                                             
         CLC   KEY+19(2),LASTEST         CHK SAME EST                           
         BNE   SP65                                                             
         BE    SP70                                                             
*                                                                               
SP65     MVI   RECTYPE,RECBUY                                                   
         MVC   FILTPUB,KEY+10                                                   
         MVC   FILTEST,KEY+19                                                   
         BAS   RE,SETSW            GO SET PROCESS SWITCHES                      
         MVC   LASTEST,KEY+19      SAVE ESTIMATE                                
         LH    RE,LASTEST                                                       
         SLL   RE,2                INDEX INTO ESTIMATE TABLE                    
         A     RE,AESTTAB                                                       
         MVC   TESTEST,3(RE)       EXTRACT TEST ESTIMATE BYTE                   
         SPACE 1                                                                
* SEE IF WE NEED TO PROCESS THIS PUB OR EST *                                   
         SPACE 1                                                                
         CLI   PUBSW,C'Y'          ANY RULE PROCESS THIS PUB                    
         BE    *+14                                                             
         MVC   KEY+16(4),XFF       FORCE NEXT PUB                               
         B     SP61                                                             
*                                                                               
         CLI   ESTSW,C'Y'          ANY RULE PROCESS THIS EST                    
         BE    *+14                                                             
         MVC   KEY+21(2),XFF       FORCE NEXT EST                               
         B     SP61                                                             
*                                                                               
         L     R2,ARULDATA                                                      
*                                                                               
SP66     CLI   QRPROC,C'Y'                                                      
         BNE   SP68                                                             
         BAS   RE,FLTPUB           GO FILTER ON FREQ                            
         BE    SP70                  AND IF PASS,PROCESS                        
*                                                                               
SP68     ICM   R2,15,QRNEXT                                                     
         BNZ   SP66                                                             
*                                                                               
SP70     MVC   AIO,AIO1                                                         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,SPGET            READ BUY RECORD                              
*                                                                               
         L     R2,ARULDATA                                                      
         XC    BUFFRULE,BUFFRULE                                                
         MVI   BUYFLAG,FIRST       SET FIRST TIME FLAG                          
         MVC   MAXSHARE,=H'10000'  INITIALIZE MAXIMUM SHARES                    
*                                                                               
SP71     CLI   QRPROC,C'Y'                                                      
         BNE   SP71X                                                            
*                                                                               
         NI    BUYFLAG,X'FF'-DRDRULE                                            
         MVC   SHARE,MAXSHARE      RE-INITIALIZE DRD SHARE                      
         BAS   RE,PROCBUY          EXTRACT BUY RECORD DATA                      
*                                                                               
         OC    BUFFRULE,BUFFRULE   DID THAT RULE USE THE RECORD                 
         BZ    SP71X               NO                                           
*                                                                               
         XC    BUFFRULE,BUFFRULE   YES                                          
         TM    BUYFLAG,FIRST+DRDRULE TEST FIRST POSTING/DRD RULE                
         BNO   *+8                                                              
         OI    BUYFLAG,DRDSHR      YES-SET DRD SHARE LOGIC IN EFFECT            
         NI    BUYFLAG,X'FF'-FIRST TURN OFF FIRST POSTING FLAG                  
         TM    BUYFLAG,DRDSHR      TEST DRDSHR FLAG                             
         BZ    SP71A               NO                                           
*                                                                               
         LH    R1,MAXSHARE         DEDUCT THIS SHARE FROM MAXIMUM               
         SH    R1,SHARE                                                         
         STH   R1,MAXSHARE                                                      
         LTR   R1,R1               TEST ANY MORE SHARE LEFT                     
         BZ    SP62                NO-READ ANOTHER BUY                          
         B     SP71X               YES-CONTINUE WITH NEXT RULE LOGIC            
*                                                                               
SP71A    ICM   R2,15,QRJUMP        NOW PICK UP JUMP POINT                       
         BNZ   SP71                YES-TRY TO POST AGAINST ANOTHER RULE         
         B     SP62                NO-READ NEXT RECORD                          
*                                                                               
SP71X    ICM   R2,15,QRNEXT        ELSE TRY ANOTHER RULE                        
         BNZ   SP71                IF ANY                                       
         B     SP62                                                             
         SPACE 1                                                                
SP72     DS    0H                                                               
         B     SP100                                                            
         EJECT                                                                  
* READ BILLING INVOICE RECORDS                                                  
*                                                                               
SP100    TM    DATAIND1,EXBADATB+EXBAINVB+EXBAADVB                              
         BZ    SP200                                                            
*                                                                               
         XC    LASTEST,LASTEST                                                  
         XC    KEY,KEY                                                          
         L     R2,ARULDATA                                                      
         LA    R4,KEY                                                           
         USING BILLRECD,R4                                                      
         MVC   PBILKAGY,QRAGYC                                                  
         MVC   PBILKMED,QRMED                                                   
         MVI   PBILKRCD,X'08'                                                   
         MVC   PBILKCLT,QRCLT                                                   
         MVC   PBILKPRD,PRD                                                     
*                                                                               
SP101    BAS   RE,SPHIGH                                                        
         B     SP104                                                            
*                                                                               
SP102    BAS   RE,SPSEQ                                                         
*                                                                               
SP104    CLC   PBILLKEY(PBILKEST-PBILLKEY),KEYSAVE TEST SAME PRD                
         BNE   SP200                               NO                           
*                                                                               
         CLC   PBILKEST,LASTEST    TEST SAME ESTIMATE                           
         BE    SP110               YES                                          
         MVI   RECTYPE,RECINV                                                   
         MVC   FILTEST,PBILKEST                                                 
         MVC   LASTEST,FILTEST                                                  
         BAS   RE,SETSW                                                         
*                                                                               
         CLI   ESTSW,C'Y'          TEST IF THIS EST SB PROCESSED                
         BE    SP110               YES                                          
         MVC   PBILKMOS(PBILLLEN-PBILKMOS),XFF  FORCE NEXT EST                  
         B     SP101                                                            
*                                                                               
SP110    L     R2,ARULDATA                                                      
         CLI   QRPROC,C'Y'         FIND FIRST RULE TO PROCESS                   
         BE    SP112                                                            
         ICM   R2,15,QRNEXT                                                     
         BNZ   *-12                                                             
         B     SP102                                                            
*                                                                               
SP112    MVC   AIO,AIO1                                                         
         BAS   RE,SPGET                                                         
         XC    BUFFRULE,BUFFRULE                                                
*                                                                               
SP114    CLI   QRPROC,C'Y'         TEST IF RULE SB PROCESSED                    
         BNE   SP115               NO-BUMP TO NEXT RULE                         
*                                                                               
         BAS   RE,PROCINV          TEST INVOICE AGAINST RULE                    
         OC    BUFFRULE,BUFFRULE   TEST IF INVOICE POSTED                       
         BZ    SP115               NO-TRY NEXT RULE                             
*                                                                               
         XC    BUFFRULE,BUFFRULE   YES                                          
         ICM   R2,15,QRJUMP        JUMP TO ANOTHER RULE                         
         BNZ   SP114                                                            
         B     SP102               NO MORE RULES TO TRY-NEXT REC                
*                                                                               
SP115    ICM   R2,15,QRNEXT                                                     
         BNZ   SP114                                                            
         B     SP102                                                            
         EJECT                                                                  
* CHANGE OF PRODUCT *                                                           
         SPACE 1                                                                
SP200    CLI   ALLSW,NO            TEST ALL PRODUCT EXTRACT                     
         BE    SP210               NO                                           
*                                                                               
         XC    KEY,KEY             YES                                          
         L     R2,ARULDATA                                                      
         MVC   KEY(2),QRAGYC                                                    
         MVC   KEY+2(1),QRMED                                                   
         MVI   KEY+3,X'06'         PRODUCT RECORD                               
         MVC   KEY+4(3),QRCLT                                                   
         MVC   KEY+7(3),PRD        GET LAST PRODUCT                             
         ZIC   R1,KEY+9            GET LAST CHARACTER OF PRODUCT                
         LA    R1,1(R1)            AND INCREMENT IT                             
         STC   R1,KEY+9            TO FORCE READ OF NEXT PRODUCT                
         BAS   RE,SPHIGH                                                        
         CLC   KEY(7),KEYSAVE      TEST SAME AGY/MED/CLIENT                     
         BNE   SP210               NO-ALL DONE WITH CLIENT                      
         MVC   PRD,KEY+7           EXTRACT NEW PRODUCT CODE                     
         B     SP14                AND GO BACK AND PROCESS                      
*                                                                               
SP210    DS    0H                                                               
         B     SP900               GO WRITE WORKER RECORDS                      
         EJECT                                                                  
* WRITE WORKER RECORDS *                                                        
         SPACE 1                                                                
SP900    L     R2,ARULDATA         R2=A(RULE ENTRY)                             
*                                                                               
SP902    ZIC   R4,NEXTYPS          R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPE TABLE)                     
         XC    BUFFREC,BUFFREC                                                  
         ST    R2,BUFFRULE                                                      
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         GOTO1 VDATAHD                                                          
*                                                                               
SP904    MVC   BUFFTYPE,0(R5)      SET EXTRACT TYPE                             
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
SP906    MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         BAS   RE,BUFFGET                                                       
         CLI   DMCB+8,0            TEST IF ITEM FOUND                           
         BE    SP908               YES                                          
         ZAP   BUFFGRS,=P'0'       NO-MANUFACTURE ZERO RECORD                   
         ZAP   BUFFNET,=P'0'                                                    
         ZAP   BUFFSPTS,=P'0'                                                   
*                                                                               
SP908    MVI   DMCB,BUPPUT                                                      
         TM    WHEN,X'18'          TEST OVERNIGHT                               
         BNZ   SP910               YES                                          
         MVI   DMCB,BUPADD                                                      
         TM    WHEN,X'20'          TEST SOON                                    
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SP910    GOTO1 VBUPPER                                                          
         LA    R6,SVEXL(R6)        NEXT PERIOD                                  
         OC    SVEXPER,SVEXPER     TEST FOR LAST PERIOD                         
         BNZ   SP906               NO                                           
*                                                                               
         LA    R5,1(R5)            NEXT EXTRACT TYPE                            
         BCT   R4,SP904                                                         
*                                                                               
         MVI   DMCB,BUPFINAL       FINAL CALL FOR OUTLINE                       
         GOTO1 VBUPPER                                                          
         ICM   R2,15,QRNEXT                                                     
         BNZ   SP902                                                            
         B     EXIT                NO MORE RULES                                
         DROP  R6                                                               
         EJECT                                                                  
PROCBUY  NTR1                                                                   
         L     R6,AIO                                                           
         USING PBUYRECD,R6                                                      
*                                                                               
         MVC   ADVDATE,PBDBDATE    SET ADV. DATE=BILLABLE DATE                  
         TM    PLANIND,BUPLNBIL    TEST FOR BILLABLE DATE OPTION                
         BO    *+10                YES                                          
         MVC   ADVDATE,PBUYKDAT    NO-USE INSERTION DATE INSTEAD                
*                                                                               
         TM    PLANIND,BUPLNTES    TEST TO SKIP TEST ESTIMATES                  
         BZ    *+12                NO                                           
         TM    TESTEST,X'80'       TEST ESTIMATE ?                              
         BO    NEQXIT              YES                                          
*                                                                               
         TM    PLANIND,BUPLNTBU    TEST TO SKIP ALL TEST BUYS                   
         BZ    *+12                NO                                           
         CLI   PBDBFD,C'T'         TEST FOR TEST BUY                            
         BE    NEQXIT              YES-FILTER OUT BUY                           
*                                                                               
         SR    R7,R7                                                            
         ICM   R7,3,QRADCDSP       TEST ADCODE FILTER                           
         BZ    PB4                                                              
         AR    R7,R2                                                            
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
PB2      CLC   PBDJOB,0(R7)                                                     
         BE    PB4                                                              
         LA    R7,6(R7)                                                         
         BCT   R8,PB2                                                           
         B     NEQXIT                                                           
*                                                                               
PB4      DS    0H                                                               
*                                  CHK ADV DATE W/IN PLAN DATES                 
         CLC   ADVDATE,SVEXTSTB                                                 
         BL    PB5                                                              
         CLC   ADVDATE,SVEXTNDB                                                 
         BH    PB5                                                              
         B     PB6                 WITHIN PLAN DATES                            
*                                  OUTSIDE PLAN DATES                           
PB5      CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BNE   NEQXIT              NO-EXIT                                      
         TM    DATAIND,EXBDATB+EXBNDATB+EXBINVB+EXBNINVB                        
         BZ    NEQXIT              NO                                           
         GOTO1 VGETINS,DMCB,PBUYREC,MYGROSS,KEY+7                               
         BAS   RE,TSTDRD                                                        
         B     PB65                BDATE PROCESSING                             
*                                                                               
PB6      GOTO1 VGETINS,DMCB,PBUYREC,MYGROSS,KEY+7                               
         TM    PBUYCNTL,X'80'      TEST FOR DELETED INSERTION                   
         BZ    *+10                                                             
         XC    MYGROSS(12),MYGROSS ZERO OUT ORDERED FIELDS                      
*                                                                               
* IF RULE IS COMBINED SINGLE PUB AND DRD, FIND THE SHARE AND                    
* APPLY IT TO ORDERED VALUES                                                    
*                                                                               
         BAS   RE,TSTDRD           TEST FOR DRD RULE                            
         TM    BUYFLAG,DRDRULE+DRDSHR                                           
         BZ    PB10                NO                                           
         GOTO1 APPSHR,DMCB,MYGROSS,3                                            
*                                                                               
*        FIND PERIOD CONTAINING ADVERTISING DATE (BILLABLE/INSERTION)           
*                                                                               
PB10     LA    R1,SVEXTDTS                                                      
         USING SVEXD,R1                                                         
PB44     CLC   ADVDATE,SVEXSTB     TEST PRIOR TO PERIOD START                   
         BL    PB50                                                             
         CLC   ADVDATE,SVEXENDB    OR AFTER PERIOD END                          
         BH    PB50                                                             
         B     PB52                                                             
PB50     LA    R1,SVEXL(R1)        NEXT PERIOD                                  
         B     PB44                                                             
*                                                                               
PB52     ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         MVC   HALF,DATAINDS                                                    
         NC    HALF,ORDEREDB       TEST IF ORDERED NEEDED                       
         BZ    PB60                                                             
         MVI   BUFFTYPE,EXORD                                                   
         L     R0,MYGROSS                                                       
         TM    PLANIND,BUPLNSCD    TEST FOR SUBTRACTING CASH DISCOUNT           
         BZ    *+8                                                              
         S     R0,CSHDSC           SUBTRACT CASH DISCOUNT FROM GROSS            
*                                                                               
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         L     R1,AGYCOM                                                        
         SR    R0,R1                                                            
         CVD   R0,DUB                                                           
         ZAP   BUFFNET,DUB                                                      
         ZAP   BUFFSPTS,=P'1'      ONE INSERTION                                
         TM    DATAIND,EXORDB      TEST FOR GROSS ORDERED                       
         BZ    *+8                                                              
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXORDNB     TEST FOR NET ORDERED                         
         BZ    PB55                                                             
         MVI   BUFFTYPE,EXORDN                                                  
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
PB55     TM    DATAIND1,EXORDDB    TEST FOR CASH DISCOUNT ORDERED               
         BZ    PB60                                                             
         MVI   BUFFTYPE,EXORDD                                                  
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         ZAP   BUFFNET,BUFFGRS                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
PB60     TM    DATAIND,EXBADVB+EXBNADVB SEE IF DOING BILLED ADV PERIOD          
         BNZ   *+12                                                             
         TM    DATAIND1,EXBDADVB                                                
         BZ    PB65                                                             
*                                                                               
* IF PUB+DRD RULE HAS BEEN POSTED AGAINST OR WILL BE, APPLY THE                 
* DRD SHARE TO THE BILLED VALUES                                                
*                                                                               
         TM    BUYFLAG,DRDRULE+DRDSHR                                           
         BZ    PB61                                                             
         GOTO1 APPSHR,DMCB,BGROSS,3                                             
*                                                                               
PB61     L     R0,BGROSS                                                        
         TM    PLANIND,BUPLNSCD                                                 
         BZ    *+8                                                              
         S     R0,BCSHDSC                                                       
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         L     R1,BAGYCOM                                                       
         SR    R0,R1                                                            
         CVD   R0,DUB                                                           
         ZAP   BUFFNET,DUB                                                      
         ZAP   BUFFSPTS,=P'1'      ONE INSERTION                                
*                                                                               
PB62     TM    DATAIND,EXBADVB                                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBADV                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBNADVB                                                 
         BZ    PB63                                                             
         MVI   BUFFTYPE,EXBNADV                                                 
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
PB63     TM    DATAIND1,EXBDADVB                                                
         BZ    PB65                                                             
         MVI   BUFFTYPE,EXBDADV                                                 
         L     R0,BCSHDSC                                                       
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         ZAP   BUFFNET,BUFFGRS                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
PB65     TM    DATAIND,EXBDATB+EXBNDATB+EXBINVB+EXBNINVB                        
         BNZ   *+12                                                             
         TM    DATAIND1,EXBDDATB                                                
         BZ    PB80                DATE                                         
*        FIND PERIOD CONTAINING BILLED DATE                                     
*                                                                               
         DROP  R6                                                               
         L     R7,AIO                                                           
         USING PBUYREC,R7                                                       
         LA    R6,PBDELEM         POINT OT FIRST ELEM                           
         MVI   ELCDLO,X'26'        MUST DO EACH BILLING ELEM                    
*                                                                               
PB70     BAS   RE,NEXTEL                                                        
         BNE   PB80                NO ELEMS OR DONE                             
         USING PBILELEM,R6                                                      
         CLC   =C'ZZZ',PRD         TEST EXTRACTING UNDER 'ZZZ'                  
         BE    PB72                YES-TAKE ALL PRODUCTS                        
         CLC   PBPRD,PRD           TEST FOR MATCH ON PRODUCT                    
         BNE   PB70                NO-SKIP ELEMENT                              
*                                                                               
PB72     TM    DATAIND,EXBINVB+EXBNINVB TEST BINV REQUESTED                     
         BZ    PB74                NO                                           
         BAS   RE,GETBILL          GET INVOICE ENTRY FOR BILL                   
         BNE   PB74                ITS OUTSIDE EXTRACT PERIOD                   
*                                                                               
         LA    R5,INVENT                                                        
         USING INVTABD,R5                                                       
         LA    R1,SVEXTDTS         R1=A(EXTRACT DATE ENTRY)                     
         USING SVEXD,R1                                                         
         CLC   SVEXPER,INVPERD     FIND ENTRY                                   
         BE    *+12                                                             
         LA    R1,SVEXL(R1)                                                     
         B     *-14                                                             
*                                                                               
         TM    DATAIND,EXBINVB     TEST GROSS BINV REQUESTED                    
         BZ    PB73                                                             
         MVI   BUFFTYPE,EXBINV                                                  
         BAS   RE,BILLPUT                                                       
*                                                                               
PB73     TM    DATAIND,EXBNINVB  TEST BNINV REQUESTED                           
         BZ    PB74                                                             
         MVI   BUFFTYPE,EXBNINV                                                 
         BAS   RE,BILLPUT                                                       
*                                                                               
PB74     OC    PBLDATE,PBLDATE                                                  
         BZ    PB70                 SKIP                                        
         CLC   PBLDATE,SVEXTSTB    TEST BILLED BEFORE PLAN START                
         BL    PB70                SKIP IT                                      
         CLC   PBLDATE,SVEXTNDB    TEST BILLED AFTER PLAN END                   
         BH    PB70                                                             
*                                                                               
         LA    R1,SVEXTDTS                                                      
PB75     CLC   PBLDATE,SVEXSTB    TEST PRIOR TO PERIOD START                    
         BL    PB76                                                             
         CLC   PBLDATE,SVEXENDB   OR AFTER PERIOD END                           
         BH    PB76                                                             
         B     PB78                                                             
PB76     LA    R1,SVEXL(R1)        NEXT PERIOD                                  
         B     PB75                                                             
*                                                                               
PB78     TM    DATAIND,EXBDATB     TEST FOR GROSS BILLING                       
         BZ    *+12                NO                                           
         MVI   BUFFTYPE,EXBDATE                                                 
         BAS   RE,BILLPUT                                                       
*                                                                               
         TM    DATAIND1,EXBDDATB   TEST FOR CASH DISCOUNT BILLED                
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBDDATE                                                
         BAS   RE,BILLPUT                                                       
*                                                                               
         TM    DATAIND,EXBNDATB                                                 
         BZ    PB70                NO-GET NEXT BILLING ELEMENT                  
         MVI   BUFFTYPE,EXBNDATE                                                
         BAS   RE,BILLPUT                                                       
         B     PB70                LOOK FOR ANOTHER BILLING ELEMENT             
*                                                                               
PB80     B     EXIT                DONE WITH THIS BUY                           
         SPACE 2                                                                
* SUB-ROUTINE TO PUT THE BILL RECORD TO BUFFALO                                 
*                                                                               
BILLPUT  NTR1                                                                   
         ST    R2,BUFFRULE         SET RULE ADDRESS                             
         MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         MVC   BEGROSS(12),PBGROSS                                              
         TM    BUYFLAG,DRDRULE+DRDSHR                                           
         BZ    BILLPUT1                                                         
         GOTO1 APPSHR,DMCB,BEGROSS,3                                            
*                                                                               
BILLPUT1 ICM   R0,15,BEGROSS                                                    
         TM    PLANIND,BUPLNSCD    TEST OPTION TO SUBTRACT CD                   
         BZ    *+10                                                             
         ICM   R1,15,BECSHDSC                                                   
         SR    R0,R1                                                            
*                                                                               
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         ICM   R1,15,BEAGYCOM                                                   
         SR    R0,R1                                                            
         CVD   R0,DUB                                                           
         ZAP   BUFFNET,DUB                                                      
         ZAP   BUFFSPTS,=P'1'                                                   
*                                                                               
         CLI   BUFFTYPE,EXBDDATE   TEST FOR CD BILLED                           
         BNE   BILLPUT2                                                         
         L     R0,BECSHDSC                                                      
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         ZAP   BUFFNET,DUB                                                      
         B     BILLPUT4                                                         
*                                                                               
BILLPUT2 CLI   BUFFTYPE,EXBNDATE                                                
         BE    *+12                                                             
         CLI   BUFFTYPE,EXBNINV                                                 
         BNE   BILLPUT4                                                         
         ZAP   BUFFGRS,BUFFNET     SET GROSS=NET                                
*                                                                               
BILLPUT4 BAS   RE,BUFFPUT                                                       
*                                                                               
BILLPUTX B     EXIT                                                             
         DROP  R1,R6,R7                                                         
         SPACE 2                                                                
* SUB-ROUTINE TO APPLY DRD SHARE TO FULLWORD BUCKETS                            
*                                                                               
* AT ENTRY, P1=A(BUCKETS), P2=N'BUCKETS, SHARE=DRD SHARE (2 DECIMALS)           
*                                                                               
APPSHR   NTR1  ,                                                                
         LM    R2,R3,0(R1)         GET PARMS                                    
         LH    RE,SHARE                                                         
         L     RF,=F'10000'        100 PERCENT                                  
*                                                                               
APPSHR2  ICM   R5,15,0(R2)         GET BUCKET                                   
         MR    R4,RE               * SHARE                                      
         SLDA  R4,1                PREPARE DIVIDEND                             
         DR    R4,RF               ROUNDED DIVIDE                               
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         STCM  R5,15,0(R2)                                                      
*                                                                               
APPSHR4  LA    R2,4(R2)                                                         
         BCT   R3,APPSHR2                                                       
*                                                                               
APPSHRX  B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO TEST IF A RULE IS A DRD SHARE RULE                             
* AND TO SET THE DRD SHARE IF SO                                                
*                                                                               
* AT ENTRY, R2=A(RULE)                                                          
* ON EXIT, SHARE AND BUYFLAG IF RULE IS DRDSHR                                  
*                                                                               
TSTDRD   ST    RE,SAVERE                                                        
         LH    R0,NSHRENT          R0=TABLE ENTRY COUNT                         
         LTR   R0,R0                                                            
         BZ    TSTDRDX             NOTHING IN TABLE                             
         LA    RE,SHRTAB           RE=A(TABLE)                                  
*                                                                               
TSTDRD2  CLM   R2,15,0(RE)         TEST FOR MATCH ON RULE                       
         BE    TSTDRD4             YES                                          
         LA    RE,L'SHRTAB(RE)                                                  
         BCT   R0,TSTDRD2                                                       
         B     TSTDRDX             NOT IN TABLE                                 
*                                                                               
TSTDRD4  MVC   SHARE,4(RE)         EXTRACT DRD SHARE                            
         OI    BUYFLAG,DRDSHR      SET FLAG FOR DRD/SHR RULE                    
*                                                                               
TSTDRDX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS INVOICE RECORDS FOR BILLING BY INVOICE                 
* DATE - GROSS/NET/ACTUAL BILL AND ACTUAL BILL FIGURES BY ADVERTISING           
* PERIOD AND BILLING DATE.  NOTE ADVERTISING PERIOD ON INVOICE                  
* RECORDS IS BASED ON BILLABLE DATE, NOT THE INSERTION DATE.                    
*                                                                               
PROCINV  NTR1                                                                   
         L     R4,AIO                                                           
         USING BILLRECD,R4                                                      
*                                                                               
* BY ADVERTISING PERIOD                                                         
*                                                                               
PI1      TM    DATAIND1,EXBAADVB   TEST ACTUAL BILL BY ADV MONTH                
         BZ    PI3                 NO                                           
         TM    PLANIND,BUPLNBIL    YES-TEST FOR BILLABLE DATE                   
         BZ    PI3                 OPTION                                       
*                                                                               
         CLC   PBILKMOS,SVEXTSPE   TEST WITHIN EXTRACT                          
         BL    PI3                                                              
         CLC   PBILKMOS,SVEXTNPE                                                
         BH    PI3                                                              
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
PI2      CLC   PBILKMOS,SVEXPER                                                 
         BE    *+12                                                             
         LA    R6,SVEXL(R6)                                                     
         B     PI2                                                              
*                                                                               
         MVI   BUFFTYPE,EXBAADV                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
* BY INVOICE DATE                                                               
*                                                                               
PI3      TM    DATAIND1,EXBAINVB    TEST ACTUAL BILLING REQUIRED                
         BZ    PI10                                                             
         GOTO1 INVPER,PBILINVD                                                  
         BNE   PI10                INVOICE DATE OUTSIDE EXTRACT                 
         MVI   BUFFTYPE,EXBAINV                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
* BY BILL DATE                                                                  
*                                                                               
PI10     TM    DATAIND1,EXBADATB   TEST ACTUAL BILL BY BILL DATE                
         BZ    PIX                 NO                                           
         GOTO1 DATCON,PARAS,PBILLDAT,(3,DUB)                                    
         GOTO1 INVPER,DUB                                                       
         BNE   PIX                 BILLED OUTSIDE EXTRACT                       
*                                                                               
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BE    PI12                YES                                          
         TM    PLANIND,BUPLNBIL    TEST BILLABLE DATE OPTION                    
         BZ    PIX                 NO-IGNORE THIS EXTRACT TYPE                  
         CLC   PBILKMOS,SVEXTSPE   TEST BILLABLE DATE W/IN EXTRACT              
         BL    PIX                                                              
         CLC   PBILKMOS,SVEXTNPE                                                
         BH    PIX                                                              
*                                                                               
PI12     MVI   BUFFTYPE,EXBADAT                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
PIX      B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO FIND THE PERIOD, AT ENTRY R1=A(DATE(YMD)), ON EXIT             
* CC=EQ IF WITHIN EXTRACT AND R6=A(DATE TABLE ENTRY), CC=NEQ IF                 
* OUTSIDE EXTRACT                                                               
*                                                                               
INVPER   ST    RE,SAVERE                                                        
         MVC   FULL(3),0(R1)                                                    
         CLC   FULL(3),SVEXTSTB     TEST IF WITHIN EXTRACT                      
         BL    INVPERN                                                          
         CLC   FULL(3),SVEXTNDB                                                 
         BH    INVPERN                                                          
*                                                                               
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
INVPER2  CLC   FULL(3),SVEXSTB     LOCATE THE PERIOD                            
         BL    INVPER4                                                          
         CLC   FULL(3),SVEXENDB                                                 
         BH    INVPER4                                                          
         B     INVPERY                                                          
*                                                                               
INVPER4  LA    R6,SVEXL(R6)        NEXT DATE ENTRY                              
         B     INVPER2                                                          
*                                                                               
INVPERY  CR    RB,RB               SET CC=EQ                                    
         B     INVPERX                                                          
*                                                                               
INVPERN  LTR   RB,RB                                                            
*                                                                               
INVPERX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO BUILD BUFFALO RECORD AND PUT TO BUFFALO                        
*                                                                               
INVPUT   NTR1                                                                   
         USING SVEXD,R6                                                         
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,SVEXPER                                                  
         CLI   BUFFTYPE,EXBINV     TEST GROSS/NET FIGURES REQUESTED             
         BE    INVPUT2                                                          
         CLI   BUFFTYPE,EXBNINV                                                 
         BE    INVPUT2                                                          
*                                                                               
INVPUT1  ZAP   BUFFGRS,PBILLRCV    ACTUAL BILL                                  
         ZAP   BUFFNET,BUFFGRS                                                  
         B     INVPUT4                                                          
*                                                                               
INVPUT2  ZAP   BUFFGRS,PBILLBIL    GROSS-CD                                     
         TM    PLANIND,BUPLNSCD    TEST IF CD SB SUBTRACTED                     
         BO    *+10                YES                                          
         ZAP   BUFFGRS,PBILLGRS    NO-USE FULL GROSS                            
         ZAP   BUFFNET,PBILLNET    NET=GROSS-COMM-CD                            
         TM    PLANIND,BUPLNSCD                                                 
         BO    INVPUT3                                                          
         ZAP   DUB,PBILLGRS                                                     
         SP    DUB,PBILLBIL        COMPUTE CD                                   
         AP    BUFFNET,DUB         ADD THE CD BACK IN TO NET                    
*                                                                               
INVPUT3  CLI   BUFFTYPE,EXBNINV    TEST FOR NET EXTRACT                         
         BNE   *+10                                                             
         ZAP   BUFFGRS,BUFFNET     YES-SET GROSS=NET                            
*                                                                               
INVPUT4  ZAP   BUFFSPTS,=P'1'                                                   
         BAS   RE,BUFFPUT                                                       
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* ELEMENT SEARCH SUB-ROUTINE                                                    
*                                                                               
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BNE   NEXTEL                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
NEXTELX  LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
         EJECT                                                                  
* SUBROUTINE TO READ PUB RECORD *                                               
         SPACE 1                                                                
FINDPUB  NTR1                                                                   
         L     R2,ARULDATA         POINT TO FIRST RULE                          
         XC    PKEY,PKEY                                                        
         MVC   PKEY(1),KEY+2       MEDIA                                        
         MVC   PKEY+1(6),KEY+10    PUB                                          
         MVC   PKEY+7(2),QRAGYC                                                 
         MVI   PKEY+9,X'81'                                                     
FINDP5   GOTO1 PUBHIGH                                                          
         CLC   PKEY(10),SVPKEY                                                  
         BE    FINDP10                                                          
         CLI   SVAPROF+16,C'0'      SEE IF I SHOULD LOOK FOR DEFAULT            
         BNE   *+6                                                              
FINDP5X  DC    H'0'                 FATAL ERR MUST FIND PUB                     
*                                                                               
         CLC   SVPKEY+7(2),=C'ZZ'   SEE IF I ALREADY LOOKED FOR ZZ              
         BE    FINDP5X              FATAL ERROR                                 
         MVC   PKEY,SVPKEY                                                      
         MVC   PKEY+7(2),=C'ZZ'                                                 
         B     FINDP5                                                           
*                                                                               
FINDP10  L     R6,APUBIO                                                        
         GOTO1 PUBGET                                                           
         L     R6,ALTLIO           LITTLE PUB REC                               
         XC    0(35,R6),0(R6)                                                   
         MVC   PKEY+7(2),QRAGYC                                                 
         MVI   PKEY+9,X'85'                                                     
         GOTO1 PUBHIGH                                                          
         CLC   PKEY(10),SVPKEY                                                  
         BNE   FINDPX                                                           
         GOTO1 PUBGET                GET LITTLE RECORD                          
FINDPX   B     EXIT                                                             
         SPACE 2                                                                
* FILTER PUB ON  REP IF REQUIRED *                                              
         SPACE 1                                                                
FLTPUB   NTR1                                                                   
         L     R6,APUBIO          WAS AIO2 (WHY?)                               
         LA    R6,33(R6)                                                        
*                                                                               
*                                                                               
FLTPUB4  SR    R7,R7                                                            
         ICM   R7,3,QRFRQDSP       TEST FREQUENCY FILTER                        
         BZ    EQXIT                                                            
         CLI   QRMED,C'N'                                                       
         BE    NEQXIT              NO FREQ FOR NEWSPAPERS                       
*                                                                               
         MVI   ELCDLO,X'20'        MUST FIND PRODUCTION ELEM                    
         BAS   RE,NEXTEL                                                        
         BNE   NEQXIT              ELSE SKIP THIS BUY                           
         USING PUBGENEL,R6                                                      
         AR    R7,R2                                                            
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
*                                                                               
FLTPUB6  CLC   PUBMFREQ(1),0(R7)                                                
         BE    EQXIT                                                            
         LA    R7,1(R7)                                                         
         BCT   R8,FLTPUB6                                                       
         B     NEQXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
* SUBROUTINE TO DETERMINE PUB/EST SHOULD BE PROCESSED *                         
         SPACE 1                                                                
SETSW    NTR1                                                                   
         MVI   ESTSW,C'N'          RESET MASTER SWITCHES                        
         MVI   PUBSW,C'N'                                                       
         XC    NSHRENT,NSHRENT     CLEAR DRD SHARE TABLE/COUNT                  
         LA    RE,SHRTAB                                                        
         LA    RF,MAXSHR*L'SHRTAB                                               
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,ARULDATA                                                      
SETSW1   MVI   QRPUBSW,C'N'                                                     
         MVI   QRESTSW,C'N'                                                     
         MVI   QRPROC,C'N'         RESET ALL RULE PROCESS SWITCHES              
         ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW1                                                           
*                                                                               
         CLI   RECTYPE,RECINV      TEST FOR INVOICE RECORD                      
         BE    SETSW48             YES-SKIP PUB PROCESSING                      
         L     R2,ARULDATA                                                      
SETSW2   ICM   RE,15,QRADRMGR      TEST PUB LIST IN THIS RULE                   
         BZ    SETSW10                                                          
         AR    RE,R2                                                            
*                                                                               
SETSW4   OC    0(4,RE),0(RE)       TEST EOL                                     
         BZ    SETSW10                                                          
         CLC   FILTPUB(4),0(RE)    PUB - BASE NUMBER                            
         BNE   SETSW6                                                           
         CLC   4(2,RE),XFF                                                      
         BE    SETSW5                                                           
         CLC   FILTPUB,0(RE)       MATCH THROUGH ZONE + EDT                     
         BNE   SETSW6              NO                                           
SETSW5   MVI   QRPUBSW,C'Y'        SET PUB SWITCH                               
         B     SETSW10                                                          
*                                                                               
SETSW6   LA    RE,6(RE)            NEXT PUB                                     
         B     SETSW4                                                           
*                                                                               
SETSW10  ICM   R2,15,QRNEXT        NEXT RULE                                    
         BNZ   SETSW2                                                           
         SPACE 1                                                                
*                                                                               
* TEST FOR SPECIFIC PUBS           6 BYTE ENTRIES IN QRPUBDSP                   
         L     R2,ARULDATA                                                      
*                                                                               
SETSW22  SR    R7,R7                                                            
         ICM   R7,3,QRPUBDSP       TEST PUB LIST                                
         BZ    SETSW30                                                          
         AR    R7,R2               POINT TO LIST                                
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)            POINT TO FIRST PUB                           
*                                                                               
SETSW24  CLC   FILTPUB(4),0(R7)    MATCH PUB - BASE NUMBER                      
         BNE   SETSW26                                                          
         CLC   4(2,R7),XFF         FOR ALL ZONES AND EDTS                       
         BE    SETSW25                                                          
         CLC   FILTPUB,0(R7)       MATCH THROUGH ZONE AND EDT                   
         BNE   SETSW26                                                          
SETSW25  MVI   QRPUBSW,C'Y'                                                     
         B     SETSW30                                                          
*                                                                               
SETSW26  LA    R7,6(R7)            NEXT PUB                                     
         BCT   R8,SETSW24                                                       
*                                                                               
SETSW30  ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW22                                                          
*                                                                               
* TEST FOR SPECIFIC DRDS          12 BYTE ENTRIES IN QRDRDDSP                   
         L     R2,ARULDATA                                                      
*                                                                               
SETSW32  SR    R7,R7                                                            
         ICM   R7,3,QRDRDDSP       DRD LIST                                     
         BZ    SETSW39                                                          
*                                                                               
* THERE IS A VALID BUSINESS SITUATION WHERE BOTH PUB AND                        
* DRD RULES CAN BE PRESENT.                                                     
*                                                                               
         OC    QRPUBDSP,QRPUBDSP   TEST FOR PUB RULE                            
         BNZ   *+14                YES                                          
         OC    QRADRMGR,QRADRMGR   TEST FOR PUB LIST RULE                       
         BZ    SETSW33             NO                                           
*                                                                               
         CLI   QRPUBSW,C'N'        SKIP DRD TEST IF MORE SPECIFIC               
         BE    SETSW39             PUB TYPE FILTER FAILED                       
         MVI   QRPUBSW,C'N'        TURN OFF PUB SWITCH                          
*                                                                               
SETSW33  AR    R7,R2               POINT TO LIST                                
         ZIC   R8,0(R7)            NUMBER OF ENTRIES                            
         LA    R7,1(R7)            POINT TO FIRST DRD                           
SETSW34  L     R6,ALTLIO                                                        
         OC    0(20,R6),0(R6)      SEE IF I HAVE A LITTLE REC                   
         BZ    SETSW39                                                          
         LA    R6,33(R6)                                                        
         MVI   ELCDLO,X'71'                                                     
         CLI   0(R6),X'71'         MIGHT BE FIRST ELEM                          
         BE    SETSW34C                                                         
SETSW34B BAS   RE,NEXTEL                                                        
         BNE   SETSW36                                                          
         USING PUBDSTEL,R6                                                      
SETSW34C OC    QRCLDDSP,QRCLDDSP   CHK FOR CLIENT OVERRIDE                      
         BNZ   SETSW34D                                                         
         CLC   PUBDCLT,QRCLT                                                    
         BE    SETSW34E                                                         
         B     SETSW34B                                                         
*                                                                               
SETSW34D SR    R1,R1                                                            
         ICM   R1,3,QRCLDDSP                                                    
         AR    R1,R2                                                            
         CLC   PUBDCLT,1(R1)                                                    
         BNE   SETSW34B                                                         
SETSW34E CLC   PUBDDIV(6),0(R7)  MATCH DIV AND REGION                           
         BNE   SETSW34B                                                         
         CLC   6(6,R7),=C'000999'    ALL DISTRICTS                              
         BE    SETSW35                                                          
         CLC   PUBDDST,6(R7)       CHK RANGE OF DISTRICTS                       
         BL    SETSW34B                                                         
         CLC   PUBDDST,9(R7)                                                    
         BH    SETSW34B                                                         
*                                                                               
* IF SPECIFIC DRD RULE AND PLAN INDICATOR TO APPLY DRD SHARE IS                 
* PRESENT, UPDATE SHARE RULE TABLE                                              
*                                                                               
         CLC   6(3,R7),9(R7)       TEST FOR SPECIFIC DRD RULE                   
         BNE   SETSW35             NO                                           
         TM    PLANIND+1,BUPLNSHR  TEST IF APPLYING DRD SHARE                   
         BZ    SETSW35             NO                                           
*                                                                               
         LH    R1,NSHRENT                                                       
         LR    RE,R1                                                            
         MH    RE,=Y(L'SHRTAB)                                                  
         LA    RE,SHRTAB(RE)       INDEX INTO SHARE TABLE                       
         LA    R1,1(R1)            INCREMENT TABLE ENTRY COUNT                  
         CH    R1,=Y(MAXSHR)       TEST FOR TABLE OVERFLOW                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         STH   R1,NSHRENT                                                       
         STCM  R2,15,0(RE)         SAVE A(RULE)                                 
         MVC   4(2,RE),PUBDSHR     SAVE SHARE                                   
         OC    PUBDSHR,PUBDSHR     TEST FOR ZERO SHARE                          
         BNZ   *+10                                                             
         MVC   4(2,RE),=H'10000'   YES-FUDGE TO 100                             
*                                                                               
         DROP  R6                                                               
*                                                                               
SETSW35  MVI   QRPUBSW,C'Y'                                                     
         B     SETSW39                                                          
*                                                                               
SETSW36  LA    R7,12(R7)            NEXT DRD                                    
         BCT   R8,SETSW34                                                       
*                                                                               
SETSW39  ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW32                                                          
*                                                                               
SETSW40  DS    0H                                                               
         L     R2,ARULDATA                                                      
SETSW42  OC    QRADRMGR,QRADRMGR         TEST PUB LIST SPECIFIED                
         BNZ   SETSW44                                                          
         OC    QRPUBDSP,QRPUBDSP          TEST PUB SPECIFIED                    
         BNZ   SETSW44                                                          
         OC    QRDRDDSP,QRDRDDSP                                                
         BNZ   SETSW44                                                          
*                                                                               
         MVI   QRPUBSW,C'Y'                PROCESS BY DEFAULT                   
SETSW44  ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW42                                                          
*                                                                               
*                                                                               
         SPACE 1                                                                
* NOW SEE IF ESTIMATE IS ACTIVE *                                               
         SPACE 1                                                                
SETSW48  L     R2,ARULDATA                                                      
*                                                                               
SETSW50  OC    QRESTDSP,QRESTDSP   TEST ESTIMATE LIST                           
         BZ    SETSW54             NO - GO FILTER                               
         SR    R7,R7                                                            
         ICM   R7,3,QRESTDSP                                                    
         AR    R7,R2                                                            
         ZIC   R8,0(R7)                                                         
         LA    R7,1(R7)                                                         
         MVC   HALF,FILTEST                                                     
         LH    RF,HALF                                                          
*                                                                               
SETSW52  CLM   RF,3,0(R7)          TEST LESS THAN START                         
         BL    SETSW56                                                          
         CLM   RF,3,2(R7)          OR MORE THAN END                             
         BH    SETSW56                                                          
*                                                                               
SETSW54  MVC   HALF,FILTEST                                                     
         LH    RF,HALF                                                          
         BAS   RE,FLTREST                                                       
         BNE   SETSW58                                                          
         MVI   QRESTSW,C'Y'                                                     
         B     SETSW58                                                          
*                                                                               
SETSW56  LA    R7,4(R7)                                                         
         BCT   R8,SETSW52                                                       
*                                                                               
SETSW58  ICM   R2,15,QRNEXT        NEXT RULE                                    
         BNZ   SETSW50                                                          
         SPACE 1                                                                
         L     R2,ARULDATA                                                      
         CLI   RECTYPE,RECBUY      TEST FOR BUY                                 
         BE    SETSW60             YES                                          
*                                  SET SWITCHES FOR INVOICE RECORD              
SETSW59  CLI   QRPRDSW,C'Y'        SEE  DOING THIS PRD                          
         BNE   SETSW59C                                                         
         CLI   QRESTSW,C'Y'                                                     
         BNE   SETSW59C                                                         
         MVI   ESTSW,C'Y'                                                       
         MVI   QRPROC,C'Y'                                                      
SETSW59C ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW59                                                          
         B     EXIT                                                             
*                                                                               
* FINALLY, COMBINE PRDSW,PUBSW AND ESTSW AND SET PROCESS RULE TO Y/N *          
         SPACE 1                                                                
SETSW60  CLI   QRPRDSW,C'Y'        SEE IF THIS RULE USING THIS PRD              
         BNE   SETSW62                                                          
         CLI   QRPUBSW,C'Y'        TEST PROC PUB FIRST                          
         BNE   SETSW62             NO - IGNORE EST                              
         MVI   PUBSW,C'Y'          SET MASTER SWITCH                            
         CLI   QRESTSW,C'Y'        IF PROC PUB,TEST PROC EST                    
         BNE   SETSW62                                                          
         MVI   ESTSW,C'Y'          SET MASTER SWITCH                            
         MVI   QRPROC,C'Y'         AND SET TO PROCESS THIS RULE                 
*                                                                               
SETSW62  ICM   R2,15,QRNEXT                                                     
         BNZ   SETSW60                                                          
         B     EXIT                                                             
         EJECT                                                                  
FLTREST  NTR1                                                                   
         SLL   RF,2                EST X 2                                      
         A     RF,AESTTAB                                                       
         OC    0(4,RF),0(RF)       TEST ESTIMATE ACTIVE                         
         BZ    NEQXIT              NO - DONE                                    
*                                                                               
         LA    R0,3                                                             
         LA    RE,QRFLT                                                         
*                                                                               
FLTREST2 CLI   0(RE),C'*'                                                       
         BE    FLTREST6                                                         
         CLI   0(RE),C' '                                                       
         BNH   FLTREST6                                                         
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    FLTREST4            YES                                          
*                                                                               
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   NEQXIT                                                           
         B     FLTREST6            YES - GO CHK NEXT POSITION                   
*                                                                               
FLTREST4 MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)                                                       
         BE    NEQXIT                                                           
*                                                                               
FLTREST6 LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FLTREST2                                                      
         B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A TABLE OF INVOICE RECORDS FOR A PRODUCT                 
*                                                                               
BLDINV   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BILLRECD,R4                                                      
         MVC   PBILKAGY,QRAGYC                                                  
         MVC   PBILKMED,QRMED                                                   
         MVI   PBILKRCD,X'08'                                                   
         MVC   PBILKCLT,QRCLT                                                   
         MVC   PBILKPRD,PRD                                                     
*                                                                               
BLDINV2  XC    LASTEST,LASTEST                                                  
         L     R5,=A(INVTAB)                                                    
         A     R5,RELO                                                          
         USING INVTABD,R5                                                       
         SR    R2,R2               R2=TABLE ENTRY COUNT                         
*                                                                               
BLDINV4  LA    R4,KEY                                                           
         BAS   RE,SPHIGH                                                        
         B     BLDINV6                                                          
*                                                                               
BLDINV5  LA    R4,KEY                                                           
         BAS   RE,SPSEQ                                                         
*                                                                               
BLDINV6  CLC   PBILLKEY(PBILKEST-PBILLKEY),KEYSAVE                              
         BNE   BLDINV10                                                         
*                                                                               
         CLC   PBILKEST,LASTEST    TEST FOR CHANGE IN ESTIMATE                  
         BE    BLDINV8                                                          
*                                                                               
         MVI   RECTYPE,RECINV      FIND OUT IF WE WANT IT                       
         MVC   FILTEST,PBILKEST                                                 
         MVC   LASTEST,PBILKEST                                                 
         BAS   RE,SETSW                                                         
         CLI   ESTSW,C'Y'          TEST IF ESTIMATE NEEDED                      
         BE    BLDINV8             YES                                          
*                                                                               
         MVC   PBILKMOS(PBILLLEN-PBILKMOS),XFF FORCE NEXT ESTIMATE              
         B     BLDINV4                                                          
*                                                                               
BLDINV8  MVC   AIO,AIO2                                                         
         BAS   RE,SPGET                                                         
         L     R4,AIO                                                           
         GOTO1 INVPER,PBILINVD                                                  
         BNE   BLDINV5                                                          
*                                                                               
         C     R2,=A(MAXENT)                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   INVEST,PBILKEST     ADD NEW TABLE ENTRY                          
         MVC   INVMOS,PBILKMOS                                                  
         MVC   INVBMN,PBILKBMN                                                  
         MVC   INVBNO,PBILKBNO                                                  
         MVC   INVPERD,SVEXPER-SVEXD(R6) EXTRACT PERIOD                         
         LA    R5,INVTABL(R5)                                                   
         LA    R2,1(R2)            INCREMENT ENTRY COUNT                        
         B     BLDINV5                                                          
*                                                                               
BLDINV10 ST    R2,NINVENT                                                       
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET THE INVOICE TABLE ENTRY FOR A BILL ELEMENT                 
*                                                                               
* AT ENTRY, R7=A(BUY RECORD), R6=A(BILL ELEMENT)                                
* ON EXIT, CC=EQ IF DATE W/IN PLAN, CC=NEQ IF OUTSIDE PLAN                      
*                                                                               
         USING PBUYREC,R7                                                       
         USING PBILELEM,R6                                                      
GETBILL  NTR1                                                                   
         XC    INVENT,INVENT                                                    
         OC    NINVENT,NINVENT     TEST IF ANY ENTRIES                          
         BZ    GETBILLN            NO                                           
*                                                                               
         LA    R5,INVENT           BUILD SEARCH KEY                             
         USING INVTABD,R5                                                       
         MVC   INVEST,PBUYKEST                                                  
         MVC   INVMOS,PBDBDATE                                                  
         MVC   INVBMN,PBLDATE                                                   
         MVC   INVBNO,PBINVNO                                                   
*                                                                               
GETBILL2 L     R0,NINVENT                                                       
         L     R4,=A(INVTAB)                                                    
         A     R4,RELO                                                          
         GOTO1 BINSRCH,DMCB,INVTABD,(R4),(R0),INVTABL,INVKEYL,A(MAXENT)         
         CLI   DMCB,0                                                           
         BNE   GETBILLN                                                         
         L     RE,DMCB                                                          
         MVC   INVENT,0(RE)                                                     
         B     GETBILLY                                                         
*                                                                               
GETBILLN LTR   RB,RB                                                            
         B     GETBILLX                                                         
*                                                                               
GETBILLY CR    RB,RB                                                            
*                                                                               
GETBILLX B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
SPHIGH   MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   DMFILE,=C'PRTDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    SPDIR                                                            
* BUILD TRACE PARAMS                                                            
         LA    R0,KEYSAVE                                                       
         ST    R0,TRIO1                                                         
         MVI   TRIO1,25                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,33                                                         
         B     SPDIR                                                            
*                                                                               
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         MVC   DMFILE,=C'PRTDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    SPDIR                                                            
         LA    R0,KEY                                                           
         ST    R0,TRIO1                                                         
         MVI   TRIO1,25                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,33                                                         
*                                                                               
SPDIR    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,KEY                    
         LR    RE,R0                                                            
         NI    DMINBTS,X'FF'-X'08' TURN OFF PASS DELETE BIT                     
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         B     SPTRACE                                                          
*                                                                               
SPGET    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',          X        
               KEY+27,AIO,DMWORK                                                
*                                                                               
         LR    RE,R0                                                            
         NI    DMINBTS,X'FF'-X'08' TURN OFF PASS DELETE BIT                     
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         LA    R0,KEY+27                                                        
         ST    R0,TRIO1                                                         
         MVI   TRIO1,4                                                          
         L     R0,AIO                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,33                                                         
         B     SPTRACE                                                          
         EJECT                                                                  
PUBHIGH  MVC   SVPKEY,PKEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   DMFILE,=C'PUBDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    PUBDIR                                                           
* BUILD TRACE PARAMS                                                            
         LA    R0,SVPKEY                                                        
         ST    R0,TRIO1                                                         
         MVI   TRIO1,25                                                         
         LA    R0,PKEY                                                          
         ST    R0,TRIO2                                                         
         MVI   TRIO2,33                                                         
         B     PUBDIR                                                           
*                                                                               
PUBSEQ   MVC   COMMAND,=C'DMRSEQ'                                               
         MVC   DMFILE,=C'PUBDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    PUBDIR                                                           
         LA    R0,PKEY                                                          
         ST    R0,TRIO1                                                         
         MVI   TRIO1,25                                                         
         LA    R0,PKEY                                                          
         ST    R0,TRIO2                                                         
         MVI   TRIO2,33                                                         
*                                                                               
PUBDIR   LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,PKEY,PKEY                  
         LR    RE,R0                                                            
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         B     SPTRACE                                                          
*                                                                               
PUBGET   LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PUBFILE',          X        
               PKEY+27,(R6),DMWORK                                              
*                                                                               
         LR    RE,R0                                                            
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         LA    R0,PKEY+27                                                       
         ST    R0,TRIO1                                                         
         MVI   TRIO1,4                                                          
         LR    R0,R6           R6 = ADDR OF IO AREA                             
         ST    R0,TRIO2                                                         
         MVI   TRIO2,33                                                         
         B     SPTRACE                                                          
         EJECT                                                                  
*                                                                               
SPTRACE  NTR1                                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     RE,DMCB                                                          
         MVC   P(6),0(RE)                                                       
         L     RE,DMCB+4                                                        
         MVC   P+8(6),0(RE)                                                     
*                                                                               
         LA    R4,P+16                                                          
         ZIC   R0,TRIO1                                                         
         GOTO1 HEXOUT,DMCB,TRIO1,(R4),(R0),=C'TOG'                              
         A     R4,DMCB+16                                                       
         LA    R4,2(R4)                                                         
*                                                                               
         ZIC   R0,TRIO2            GET OUTPUT REC LEN                           
         GOTO1 HEXOUT,DMCB,TRIO2,(R4),(R0),=C'TOG'                              
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                RETURN TO CALLER                             
         DROP  R8                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
BUFFPUT  MVC   COMMAND(8),=CL8'BPUT'                                            
         B     GOBUFF                                                           
*                                                                               
BUFFGET  MVC   COMMAND(8),=CL8'BGET'                                            
*                                                                               
GOBUFF   LR    R0,RE               SAVE CALLING REG                             
         L     RF,TWAVBUFF                                                      
         GOTO1 (RF),DMCB,COMMAND+1,BUFFBUFF,BUFFREC,1                           
         LR    RE,R0               RESTORE CALLING REG                          
*                                                                               
         TM    SVTRACE,X'40'       TEST TRACE BUFFALO                           
         BZR   RE                                                               
*                                                                               
BUFFTRC  NTR1                                                                   
         MVC   WORK(16),DMCB       SAVE DMCB                                    
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   P(5),COMMAND                                                     
*                                                                               
         L     R4,BUFFRULE                                                      
         LA    R5,QRNODE-QRD(R4)                                                
         GOTO1 HEXOUT,DMCB,(R5),P+7,4,=C'TOG'                                   
*                                                                               
         LA    R5,QRCODE-QRD(R4)                                                
         MVC   P+17(8),0(R5)                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,BUFFREC,P+27,32,=C'TOG'                              
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+8,P+92,1,=C'TOG'                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   DMCB(16),WORK       RESTORE DMCB                                 
         B     EXIT                                                             
*                                                                               
BUFFBUFF DS    A                                                                
         SPACE 2                                                                
* TABLE OF VALID EXTRACT TYPES FOR PRINT EXTRACT                                
*                                                                               
EXTTBL   DS    0CL3                                                             
         DC    AL1(EXORD),AL1(EXORDB),AL1(0)                                    
         DC    AL1(EXBADV),AL1(EXBADVB),AL1(0)                                  
         DC    AL1(EXBDATE),AL1(EXBDATB),AL1(0)                                 
         DC    AL1(EXORDN),AL1(EXORDNB),AL1(0)                                  
         DC    AL1(EXBNADV),AL1(EXBNADVB),AL1(0)                                
         DC    AL1(EXBNDATE),AL1(EXBNDATB),AL1(0)                               
         DC    AL1(EXBINV),AL1(EXBINVB),AL1(0)                                  
         DC    AL1(EXBNINV),AL1(EXBNINVB),AL1(0)                                
         DC    AL1(EXBAADV),AL1(0),AL1(EXBAADVB)                                
         DC    AL1(EXBADAT),AL1(0),AL1(EXBADATB)                                
         DC    AL1(EXBAINV),AL1(0),AL1(EXBAINVB)                                
         DC    AL1(EXORDD),AL1(0),AL1(EXORDDB)                                  
         DC    AL1(EXBDADV),AL1(0),AL1(EXBDADVB)                                
         DC    AL1(EXBDDATE),AL1(0),AL1(EXBDDATB)                               
EXTYPES  EQU   (*-EXTTBL)/L'EXTTBL                                              
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BADAGY   LA    R4,AGYMSG                                                        
         B     BAD2                                                             
*                                                                               
BADCLT   LA    R4,CLTMSG                                                        
         B     BAD2                                                             
*                                                                               
BADPRD   LA    R4,PRDMSG                                                        
         B     BAD2                                                             
*                                                                               
BADDIV   DC    H'0'                                                             
BADREG   DC    H'0'                                                             
BADDST   DC    H'0'                                                             
BADLST   DC    H'0'                                                             
BADPUB   DC    H'0'                                                             
*                                                                               
BADEST   LA    R4,ESTMSG                                                        
         B     BAD2                                                             
*                                                                               
BAD2     GOTO1 VPRTRULE,PARAS,0                                                 
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVC   P(8),QRCODE                                                      
         MVC   P+9(7),=C'INVALID'                                               
         ZIC   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+17(0),1(R4)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
XFF      DC    16X'FF'                                                          
RELO     DC    A(0)                                                             
*                                                                               
BINSRCH  DS    V                                                                
APUBIO   DS    A                                                                
ALTLIO   DS    A                                                                
PKEY     DS    CL32                                                             
SVPKEY   DS    CL32                                                             
SVAPROF  DS    CL30                                                             
PRDSW    DS    C                                                                
PUBSW    DS    C                                                                
ESTSW    DS    C                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
ALLSW    DS    C                   Y=ALL PRODUCT EXTRACT                        
*                                                                               
PRD      DS    CL3                 PRODUCT BEING PROCESSED                      
DIV      DS    CL3                 DIVISION FOR PRD                             
LASTEST  DS    H                                                                
TESTEST  DS    XL1                 X'80'=TEST ESTIMATE                          
*                                                                               
DATAINDS DS    0XL2                                                             
DATAIND  DS    X                                                                
DATAIND1 DS    X                                                                
NEXTYPS  DS    X                   N'EXTRACT TYPES                              
EXTYPS   DS    CL(MAXDTYP)         EXTRACT TYPE TABLE                           
*                                                                               
ORDEREDB DS    H                                                                
         ORG   ORDEREDB                                                         
         DC    AL1(EXORDB+EXORDNB)                                              
         DC    AL1(EXORDDB)                                                     
BILLEDB  DS    H                                                                
         ORG   BILLEDB                                                          
         DC    AL1(EXBADVB+EXBNADVB+EXBDATB+EXBNDATB+EXBINVB+EXBNINVB)          
         DC    AL1(EXBDADVB+EXBDDATB)                                           
BUYRB    DS    H                                                                
*                                                                               
RECTYPE  DS    X                                                                
FILTPUB  DS    CL6                                                              
FILTEST  DS    XL2                                                              
*                                                                               
ADVDATE  DS    XL3                 ADVERTISING DATE                             
*                                                                               
MAXSHARE DS    H                                                                
SHARE    DS    H                                                                
BUYFLAG  DS    XL1                                                              
FIRST    EQU   X'80'                                                            
DRDRULE  EQU   X'40'                                                            
DRDSHR   EQU   X'20'               DRD SHARE APPLICATION IN EFFECT              
MAXSHR   EQU   50                  MAXIMUM SIZE OF SHARE TABLE                  
NSHRENT  DS    H                                                                
SHRTAB   DS    (MAXSHR)XL6         RULE(4)/SHARE(2)                             
*                                                                               
INVENT   DC    XL(INVTABL)'00'                                                  
NINVENT  DC    F'0'                                                             
*                                                                               
AEXTDTS  DS    A                                                                
LASTKEY  DC    XL(L'PBILLKEY)'00'                                               
SAVEKEY  DS    CL(L'KEY)                                                        
*                                                                               
AGYMSG   DC    AL1(06),C'AGENCY'                                                
CLTMSG   DC    AL1(06),C'CLIENT'                                                
PRDMSG   DC    AL1(07),C'PRODUCT'                                               
ESTMSG   DC    AL1(08),C'ESTIMATE'                                              
*                                                                               
*        GETINS (PETRATE) OUTPUT PARAMETER BLOCK                                
* ORDERED DATA                                                                  
MYGROSS  DS    F                   GROSS ORDERED                                
AGYCOM   DS    F                   AGENCY COMMISSION                            
CSHDSC   DS    F                   CASH DISCOUNT                                
PYABLE   DS    F                   GROSS-AGYCOMM-CASHDSC                        
BLABLE   DS    F                   GROSS-CASH DSC                               
PREMIUM  DS    F                   (INCLUDED IN ABOVE FIELDS)                   
UNITS    DS    F                   NUMBER OF LINES BOUGHT                       
* PAID DATA                                                                     
PGROSS   DS    F                   GROSS PAID                                   
PAGYCOM  DS    F                   AGY COMM PAID                                
PCSHDSC  DS    F                   CASH DISCOUNT PAID                           
PAID     DS    F                   ACTUAL PAID AMOUNT                           
PYBLDT   DS    CL3                 PAYABLE DATE -YMD                            
* BILLED DATA                                                                   
BGROSS   DS    F                   GROSS BILLED                                 
BAGYCOM  DS    F                   AGY COMM BILLED                              
BCSHDSC  DS    F                   CASH DISCOUNT BILLED                         
BILLED   DS    F                   ACTUAL BILLED AMOUNT                         
BLBLDT   DS    CL3                 BILLABLE DATE -YMD                           
*                                                                               
* BILLING ELEMENT DATA                                                          
*                                                                               
BEGROSS  DS    F                                                                
BEAGYCOM DS    F                                                                
BECSHDSC DS    F                                                                
*                                                                               
*                                                                               
* EQUATES                                                                       
*                                                                               
RECBUY   EQU   X'01'                                                            
RECINV   EQU   X'02'                                                            
MAXENT   EQU   15000                                                            
         SPACE 2                                                                
         BUFF  LINES=250,ROWS=1,COLUMNS=3,FLAVOR=PACKED,               X        
               KEYLIST=(8,A)                                                    
         SPACE 2                                                                
INVTAB   DC    (MAXENT)XL(INVTABL)'00'                                          
         SPACE 2                                                                
* DSECT TO COVER INVOICE TABLE                                                  
*                                                                               
INVTABD  DSECT                                                                  
INVEST   DS    XL2                 ESTIMATE NUMBER                              
INVMOS   DS    XL2                 MONTH-OF-SERVICE                             
INVBMN   DS    XL2                 BILLING YEAR/MONTH                           
INVBNO   DS    XL2                 BILL NUMBER                                  
INVKEYL  EQU   *-INVTABD                                                        
INVPERD  DS    XL2                                                              
INVTABL  EQU   *-INVTABD                                                        
         EJECT                                                                  
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
       ++INCLUDE BUEXTWORKD                                                     
         EJECT                                                                  
QRPUBSW  EQU     QRSW1                                                          
*                           QRESTSW IS QRSW2                                    
QRPRDSW  EQU     QRSW3                                                          
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
       ++INCLUDE DDCOMFACS                                                      
PAGYRECD DSECT                                                                  
       ++INCLUDE PAGYREC                                                        
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
BILLELEM DSECT                                                                  
       ++INCLUDE PBILELEM                                                       
BILLRECD DSECT                                                                  
       ++INCLUDE PBILLREC                                                       
*                                                                               
*PRODUCTION ELEM                                                                
       ++INCLUDE PUBGENEL                                                       
         PRINT ON                                                               
*                                                                               
PUBDSTD  DSECT                                                                  
       ++INCLUDE PUBDSTEL                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089BUFIL44   05/01/02'                                      
         END                                                                    
