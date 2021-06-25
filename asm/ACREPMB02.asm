*          DATA SET ACREPMB02  AT LEVEL 005 AS OF 10/03/03                      
*PHASE ACMB02A                                                                  
***********************************************************************         
*              OPT1:'D'= RUN DRAFT ONLY                               *         
*              OPT2:' '= RUN ALL AGENCIES                             *         
*                   'B'= RUN ONLY ON BATES                            *         
*                   'C'= RUN ONLY ON CAMPBELL - MITHUN                *         
*                   'D'= RUN ONLY ON SAATCHI & SAATCHI                *         
*              OPT3:' '= RUN ON ALL FILES                             *         
*                   'N'= RUN ON NETWORK FILES ONLY                    *         
*                   'P'= RUN ON PRINT   FILES ONLY                    *         
*                   'S'= RUN ON SPOT    FILES ONLY                    *         
***********************************************************************         
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE ADDTRN                                                                 
*INCLUDE UNDERLIN                                                               
         TITLE 'ZENITH MEDIA BILLING'                                           
ACMB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACMB**,R7,R9    BASE REGISTERS 11,7,9                        
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACMBD,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     RF,MCUTL                                                         
         ST    RF,AUTL                                                          
         MVC   UTL(L'UTL),0(RF)     SAVE PRESENT UTL SETTING                    
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         USING BIND,R1                                                          
         L     R1,AAGNTAB          A(AGENCY TABLE)                              
         XC    BININ,BININ         CLEAR AGENCY TABLE - BIN TABLE 1             
         L     R1,ASJTAB           A(SJ RECORD TABLE)                           
         XC    BININ,BININ         CLEAR SJ TABLE     - BIN TABLE 2             
         DROP  R1                                                               
*                                                                               
         MVC   QSTART(4),QMOSSTRT                                               
         MVC   QEND(4),QMOSEND                                                  
*                                                                               
         XC    SYSSAVE,SYSSAVE     CLEAR SAVED AREA FOR SYSTEM ID               
         XC    SVAGY,SVAGY         CLEAR SAVED AREA FOR AGENCY ID               
         ZAP   PKZERO,=P'0'                                                     
         ZAP   DMPTOT,PKZERO                                                    
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    OPTN,OPTN                                                        
         MVC   PRTDFT,SPACES       CLEAR FIELD TO SPACES                        
         CLI   QOPT1,C'D'          RUN PRINTOUT ONLY???                         
         BNE   *+14                                                             
         OI    OPTN,OPTDFT                                                      
         MVC   PRTDFT,=C'DRAFT ONLY'                                            
         CLI   QOPT2,C' '          PRINT ONLY SELECT AGENCY                     
         BE    *+14                                                             
         OI    OPTN,OPTAGY                                                      
         MVC   SVAGY,QOPT2         MOVE AGENCY INTO SAVED AREA                  
         CLI   QOPT3,C' '          RUN ON SPECIFIC SYSTEM ONLY                  
         BE    REQF20                                                           
         MVC   SYSSAVE,QOPT3                                                    
         LA    R1,SYSTAB           R1 = A(SYSTEM TABLE)                         
         USING SYSTBLD,R1                                                       
REQF10   CLI   0(R1),X'FF'                                                      
         BE    REQF20                                                           
         CLC   SYSTID,SYSSAVE                                                   
         BE    *+12                                                             
         LA    R1,SYSTLNQ(R1)      BUMP TO NEXT ENTRY                           
         B     REQF10                                                           
         OI    OPTN,OPTSYS         TURN ON BIT ONLY IF MATCH IS FOUND           
*                                                                               
REQF20   DS    0H                                                               
         XC    SVSENUM,SVSENUM     CLEAR SAVED AREA FOR SYS SE#                 
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   SVSENUM,4(R1)       SAVE SYSTEM SE NUMBER                        
         DROP  R1                                                               
         USING SYSTBLD,R2                                                       
         LA    R2,SYSTAB           R2 = A(SYSTEM TABLE)                         
         XC    SESAVE,SESAVE       CLEAR SAVED AREA FOR SE #                    
REQF30   CLI   0(R2),X'FF'         CHECK FOR END OF TABLE                       
         BE    REQFX               EXIT LOOP                                    
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),SYSTSE      MOVE SE# INTO UTL+4                          
         TM    OPTN,OPTSYS         TEST IF SELECTED SYSTEM REQUESTED            
         BNO   *+14                                                             
         CLC   SYSTID,SYSSAVE      COMPARE ON ID'S                              
         BNE   REQF50                                                           
         TM    OPTN,OPTAGY         TEST IF SELECTED SYSTEM REQUESTED            
         BNO   *+14                                                             
         CLC   SYSTAGY,SVAGY       COMPARE ON AGENCIES                          
         BNE   REQF50                                                           
         CLC   SESAVE,SYSTSE       CHECK IF SE MATCHES PREVIOUS SE              
         BE    REQF40                IF EQUAL SKIP OPEN ROUTINE                 
         SR    R5,R5                                                            
         ICM   R5,15,ASYSFIL                                                    
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',SYSTNAM,(R5)                             
REQF40   MVC   SESAVE,SYSTSE       MOVE IN NEW SE# TO SAVE AREA                 
         SR    RF,RF               INITIALIZE RF FOR BASR                       
         ICM   RF,15,ASYSRD        RF = A(SYSTEM READ ROUTINE)                  
         BASR  RE,RF               READ RECORDS                                 
REQF50   LA    R2,SYSTLNQ(R2)      BUMP TO NEXT ENTRY                           
         B     REQF30                                                           
*                                                                               
REQFX    DS    0H                                                               
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),SVSENUM     RESET SYSTEM SE NUMBER                       
         BAS   RE,XSRT             RE-SORT TABLE BY KEY AND SORT BY ID          
         BAS   RE,GETCST           GET COSTING ACCOUNT                          
         BAS   RE,CONTAB           CONVERT TABLE FOR UPDATING                   
         TM    OPTN,OPTDFT         DO THEY WANT JUST PRINTOUT???                
         BO    *+8                   IF THEY DO SKIP UPDATE ROUTINE             
         BAS   RE,UPDATE                                                        
         BAS   RE,PRTRTE           RETRIEVE RECORDS FROM 1ST TABLE              
         BAS   RE,PRTTAB           RETRIEVE RECORDS FROM 2ND TABLE              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ SPOT/NET FILES                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING SYSTBLD,R2                                                       
SPTRD    NTR1                                                                   
         XC    DKEY,DKEY                                                        
         XC    SAVEKEY,SAVEKEY     CLEAR SAVED AREA FOR KEY                     
         XC    SAVEID,SAVEID       CLEAR FIELD FOR COMPANY ID                   
         MVC   SAVEID,SYSTCODE     MOVE IN AGENCY CHARACTER CODE                
         USING SPBILD,R4                                                        
         LA    R4,DKEY             R4 = A(KEY)                                  
         MVI   BKEYTYPE,X'00'      RECORD TYPE                                  
         MVC   BKEYAM,SYSTMED      MOVE IN AGENCY/MEDIA                         
*                                                                               
SPTRD10  DS    0H                                                               
         BAS   RE,DMHISDIR         READ HI ON SPOT DIR                          
         B     SPTRD20                                                          
*                                                                               
SPTRD15  DS    0H                                                               
         BAS   RE,DMSESDIR           READ SEQ FOR SPOT DIR                      
SPTRD20  LA    R4,DIR                R4 = A(RETURNED KEY)                       
         CLC   DIR(L'BKEYTYPE),DKEY  COMPARE RETURNED KEY W/ ORIGINAL           
         BNE   SPTRDX                                                           
         MVC   SAVEAM,BKEYAM             MOVE IN AGENCY MEDIA                   
         NI    SAVEAM,X'F0'              TURNOFF ALL MEDIA BITS                 
         CLC   DKEY+1(L'BKEYAM),SAVEAM   COMPARE AGENCY W/ORIGINAL              
         BNE   SPTRDX                                                           
         OC    BKEYMBIL,BKEYMBIL   CHECK IF BILLING CODE EXISTS                 
         BZ    SPTRD15               IF NOT - READ SEQUENTIAL                   
         BAS   RE,SPBIL              BILLING ROUTINE                            
         B     SPTRD15                                                          
*                                                                               
SPTRDX   DS    0H                                                               
         BAS   RE,SYSOUT           WRITE OUTPUT RECORDS TO SORT                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* READ PRINT FILES                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SYSTBLD,R2                                                       
PRTRD    NTR1                                                                   
         XC    DKEY,DKEY           CLEAR KEY                                    
         XC    SAVEKEY,SAVEKEY     CLEAR SAVED AREA FOR KEY                     
         XC    SAVEID,SAVEID       CLEAR FIELD FOR COMPANY ID                   
         MVC   SAVEID,SYSTCODE     MOVE IN AGENCY CHARACTER CODE                
         USING PRBILD,R4                                                        
         LA    R4,DKEY             R4 = A(KEY)                                  
         LA    R3,MEDTAB           R3 = A(MEDIA CODE TABLE)                     
PRTRD10  CLI   0(R3),X'FF'                                                      
         BE    PRTRDX                                                           
         CLI   SYSTAGY,C'D'        SAATCHI RESTRICTIONS ON PRINT                
         BNE   PRTRD15                                                          
         CLC   SYSTCODE,=C'DF'       DF - NO RESTRICTIONS                       
         BE    PRTRD15                                                          
         CLI   0(R3),C'O'            LF/SF - ONLY OUTDOOR                       
         BNE   PRTRD40                                                          
*                                                                               
PRTRD15  MVC   PBILKAGY,SYSTCODE   MOVE IN AGENCY                               
         MVC   PBILKMED,0(R3)      MOVE IN MEDIA                                
         MVI   PBILKRCD,X'08'      BILLING RECORD CODE                          
*                                                                               
PRTRD20  DS    0H                                                               
         BAS   RE,DMHIPDIR         READ HIGH FOR PRINT DIR                      
         B     PRTRD30                                                          
*                                                                               
PRTRD25  DS    0H                                                               
         BAS   RE,DMSEPDIR                 READ SEQ FOR SPOT DIR                
PRTRD30  CLC   DIR(PBILKCLT-PBILLREC),DKEY  COMPARE WITH ORIGINAL KEY           
         BNE   PRTRD40                                                          
         BAS   RE,PRBIL                    PRINT ESTIMATE ROUTINE               
         B     PRTRD25                                                          
*                                                                               
PRTRD40  LA    R3,L'MEDTAB(R3)     BUMP R3 TO NEXT MEDIA CODE                   
         B     PRTRD10                                                          
*                                                                               
PRTRDX   DS    0H                                                               
         BAS   RE,SYSOUT           WRITE OUTPUT RECORDS TO SORT                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD CLIENT/PRODUCT TO TABLE FOR QUALIFYING BILLINGS  (SPOT/NET)     *         
***********************************************************************         
         SPACE 1                                                                
SPBIL    NTR1                                                                   
         BAS   RE,DMGETSPT                                                      
         USING SPBILD,R4                                                        
         L     R4,AIO1             R4 = A(IO1)                                  
         LA    R6,AGNWRK                                                        
         USING AGND,R6                                                          
         XC    AGNWRK,AGNWRK                                                    
*                                                                               
         CLC   QMOSSTRT,BDATE      CHECK IF DATE IS IN PARAMETERS               
         BH    SPBILX                IF NOT - EXIT                              
         CLC   QMOSEND,BDATE                                                    
         BL    SPBILX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(1,PKDTE)                                  
         GOTO1 DATCON,DMCB,(0,BDATE),(6,AGNMTH)                                 
*        MVC   MOSDTE(4),BMONSERV    MONTH OF SERVICE                           
*        MVC   MOSDTE+4(2),=C'01'                                               
*        GOTO1 DATCON,DMCB,(0,MOSDTE),(6,AGNMTH)                                
         GOTO1 CLUNPK,DMCB,BKEYCLT,AGNCLI                                       
         MVC   AGNPKCLI,BKEYCLT             MOVE IN CLIENT  CODE                
         MVC   AGNPRD,BKEYPRD               MOVE IN PRODUCT CODE                
         MVC   AGNAGY,SAVEID                MOVE IN AGENCY  CODE                
         MVC   AGNSPTAM,BKEYAM              MOVE IN AGENCY/MEDIA CODE           
         MVI   AGNSYS,X'80'                 MOVE IN SYSTEM FLAG - SPOT          
         MVC   AGNSE,SESAVE                 MOVE IN SE NUMBER                   
         MVC   AGNSYSID,SYSTID              MOVE IN SYSTEM ID                   
*                                                                               
* CONVERT SPOT MEDIA                                                            
*                                                                               
         MVC   SVMED,AGNSPTAM      SPOT AGENCY/MEDIA                            
         NI    SVMED,X'0F'         TURNOFF HIGH ORDER NIBBLE                    
         LA    R1,SPTTAB           SPOT MEDIA TABLE                             
SPBIL20  CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                MEDIA HAS TO BE THERE                        
         CLC   SVMED,0(R1)                                                      
         BE    *+12                                                             
         LA    R1,L'SPTTAB(R1)                                                  
         B     SPBIL20                                                          
         MVC   AGNMED,1(R1)        MOVE IN MEDIA FROM TABLE                     
*                                                                               
         MVC   AGNID,SYSTAGY                MOVE IN AGENCY ID                   
         MVC   AGNMTHPK,PKBILDTE            MOVE IN PACKED DATE                 
         ZAP   AGNBKT,BGRSP                 BILLING AMOUNT (GROSS)              
         GOTO1 BINADD,DMCB,AGNWRK,AAGNTAB                                       
*                                                                               
SPBILX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADD CLIENT/PRODUCT TO TABLE FOR QUALIFYING BILLINGS  (PRINT)        *         
***********************************************************************         
         SPACE 1                                                                
PRBIL    NTR1                                                                   
         BAS   RE,DMGETPRT                                                      
         USING PRBILD,R4                                                        
         L     R4,AIO1             R4 = A(IO1)                                  
         LA    R6,AGNWRK           R2 = A(WORK AREA FOR BINADD)                 
         USING AGND,R6                                                          
         XC    AGNWRK,AGNWRK       CLEAR WORK AREA                              
*                                                                               
         CLC   QMOSSTRT,PBILLDAT   CHECK IF DATE IS IN PARAMETERS               
         BH    PRBILX                IF NOT EXIT                                
         CLC   QMOSEND,PBILLDAT                                                 
         BL    PRBILX                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(1,PKDTE)                               
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(6,AGNMTH)     BILLING DATE P/O         
*        MVC   MOSDTE(4),PBILKMOS      MONTH OF SERVICE ROUTINE FOR P/O         
*        MVC   MOSDTE+4(2),=X'01'                                               
*        GOTO1 DATCON,DMCB,(3,MOSDTE),(6,AGNMTH)                                
         MVC   AGNCLI,PBILKCLT     MOVE CLIENT  CODE INTO WORK AREA             
         MVC   AGNPRD,PBILKPRD     MOVE PRODUCT CODE INTO WORK AREA             
         MVC   AGNAGY,PBILKAGY     MOVE AGENCY  CODE INTO WORK AREA             
         MVC   AGNMED,PBILKMED     MOVE MEDIA   CODE INTO WORK AREA             
         MVC   AGNSE,SESAVE        MOVE SE NUMBER    INTO WORK AREA             
         MVC   AGNSYSID,SYSTID     MOVE IN SYSTEM ID                            
         MVC   AGNID,SYSTAGY       MOVE IN AGENCY ID                            
         MVC   AGNMTHPK,PKBILDTE   MOVE IN BILL DATE PACKED                     
         LA    R4,33(R4)           BUMP TO FIRST ELEMENT                        
         USING PBILLEL,R4                                                       
PRBIL10  CLI   0(R4),X'00'         NO MORE ELEMENTS                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'08'         X'08' BILLING ELEMENT CODE                   
         BE    PRBIL20                                                          
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     PRBIL10                                                          
PRBIL20  ZAP   AGNBKT,PBILLGRS     MOVE IN BILLING AMOUNT                       
         GOTO1 BINADD,DMCB,AGNWRK,AAGNTAB                                       
*                                                                               
PRBILX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* PUT CLIENT/PRODUCT RECORDS TO 1ST BINTABLE                          *         
***********************************************************************         
         SPACE 1                                                                
SYSOUT   NTR1                                                                   
         L     R5,AAGNTAB                                                       
         USING BIND,R5                                                          
         SR    R0,R0                                                            
         ICM   R0,15,BININ         R0=NUMBER IN TABLE                           
         BZ    SYSOUTX             NOTHING TO PROCESS                           
         LA    R6,BINTAB                                                        
         USING AGND,R6                                                          
*                                                                               
SYSOUT10 TM    AGNSW,X'80'         TEST IF ALREADY CONVERTED                    
         BO    SYSOUT30                                                         
         TM    AGNSYS,X'80'        TEST IF SYSTEM IS PRINT OR SPOT/NET          
         BNO   SYSOUT20            IF X'80' NOT SET - PRINT                     
         BAS   RE,SPCLT            PROCESS CLIENT  RECORD (SPOT/NET)            
         BAS   RE,SPPRD            PROCESS PRODUCT RECORD (SPOT/NET)            
         B     SYSOUT30                                                         
*                                                                               
SYSOUT20 BAS   RE,PRCLT            PROCESS CLIENT  RECORD (PRINT)               
         BAS   RE,PRPRD            PROCESS PRODUCT RECORD (PRINT)               
*                                                                               
SYSOUT30 LA    R6,AGNLNQ(R6)       BUMP TO NEXT ENTRY                           
         BCT   R0,SYSOUT10                                                      
*                                                                               
SYSOUTX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS CLIENT ROUTINE  (SPOT/NET)                                  *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
SPCLT    NTR1                                                                   
         LA    R4,DKEY                                                          
         USING SPCLID,R4                                                        
         XC    DKEY,DKEY                                                        
         XC    SAVEID,SAVEID                                                    
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,AGNSPTAM     AGY/MED                                      
         MVC   CKEYCLT,AGNPKCLI    CLIENT CODE                                  
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),AGNSE       MOVE SE# INTO UTL+4                          
         BAS   RE,DMHISDIR                                                      
         CLC   DIR(CKEYZRO-CLTHDR),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETSPT                                                      
         L     R4,AIO1             R4 = A(IO1)                                  
*                                                                               
SPCLT10  DS    0H                                                               
         CLC   CZENCLT,SPACES      ANYTHING IN FIELD                            
         BNH   *+16                                                             
         MVC   AGNACLI,AGNCLI      SAVE ORIGINAL CLIENT CODE                    
         MVC   AGNCLI,CZENCLT      MOVE OVERRIDE INTO TABLE                     
*                                                                               
         MVC   AGNCNAM,CNAME       MOVE CLIENT NAME TO TABLE                    
         MVC   AGNOFF,COFFICE      MOVE IN OFFICE CODE                          
         BAS   RE,GETAGN           GET AGENCY NAME                              
*                                                                               
SPCLTX   OI    AGNSW,X'80'         TURN ON SWITCH - ENTRY WAS CONVERTED         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS PRODUCT ROUTINE  (SPOT/NET)                                 *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
SPPRD    NTR1                                                                   
         LA    R4,DKEY                                                          
         USING SPPRDD,R4                                                        
         XC    DKEY,DKEY                                                        
         XC    SAVEID,SAVEID                                                    
         MVI   PKEYTYPE,0                                                       
         MVC   PKEYAM,AGNSPTAM     AGY/MED                                      
         MVC   PKEYCLT,AGNPKCLI    CLIENT CODE                                  
         MVC   PKEYPRD,AGNPRD      PRODUCT CODE                                 
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),AGNSE       MOVE SE# INTO UTL+4                          
         BAS   RE,DMHISDIR                                                      
         CLC   DIR(PKEYZRO-PRDHDR),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETSPT                                                      
         L     R4,AIO1             R4 = A(IO1)                                  
*                                                                               
         MVC   AGNPNAM,PNAME       MOVE CLIENT NAME TO STORAGE                  
*                                                                               
SPPRDX   B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS CLIENT ROUTINE  (PRINT)                                     *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
PRCLT    NTR1                                                                   
         LA    R4,DKEY             R4 = A(KEY)                                  
         USING PRCLID,R4                                                        
         XC    DKEY,DKEY           CLEAR KEY                                    
         XC    SAVEID,SAVEID                                                    
         MVC   PCLTKAGY,AGNAGY     MOVE AGENCY CODE INTO THE KEY                
         MVC   PCLTKMED,AGNMED     MOVE MEDIA  CODE INTO THE KEY                
         MVI   PCLTKRCD,X'02'      MOVE RECORD CODE INTO THE KEY                
         MVC   PCLTKCLT,AGNCLI     MOVE CLIENT CODE INTO THE KEY                
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),AGNSE       MOVE SE# INTO UTL+4                          
         BAS   RE,DMHIPDIR         READ HIGH FROM PRINT DIR                     
         CLC   DIR(7),DKEY         COMPARE KEY WITH ORIGINAL                    
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
*                                                                               
         BAS   RE,DMGETPRT         GET RECORD FROM PRINT FILE                   
         L     R4,AIO1                                                          
         MVC   AGNCNAM,PCLTNAME    MOVE IN CLIENT NAME                          
         MVC   AGNOFF,PCLTOFF      MOVE IN OFFICE CODE                          
         BAS   RE,GETAGN           SORT ROUTINE                                 
*                                                                               
PRCLT05  LA    R4,33(R4)           BUMP TO FIRST ELEMENT                        
         USING PCLTZEL,R4                                                       
PRCLT10  CLI   0(R4),X'00'         NO MORE ELEMENTS                             
         BE    PRCLTX                                                           
         CLI   0(R4),X'32'         X'32' ZENITH ELEMENT                         
         BE    PRCLT20                                                          
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     PRCLT10                                                          
PRCLT20  CLC   PCLTZEN,SPACES      IS THERE A ZENITH CODE???                    
         BNH   PRCLTX                                                           
         MVC   AGNACLI,AGNCLI      SAVE ORIGINAL                                
         MVC   AGNCLI,PCLTZEN      MOVE IN OVERIDE ZENITH CODE                  
*                                                                               
PRCLTX   OI    AGNSW,X'80'         TURN ON SWITCH - ENTRY WAS CONVERTED         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS PRODUCT ROUTINE  (PRINT)                                    *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
PRPRD    NTR1                                                                   
         LA    R4,DKEY             R4 = A(KEY)                                  
         USING PRPRDD,R4                                                        
         XC    DKEY,DKEY           CLEAR KEY                                    
         XC    SAVEID,SAVEID                                                    
         MVC   PPRDKAGY,AGNAGY     MOVE AGENCY  CODE INTO THE KEY               
         MVC   PPRDKMED,AGNMED     MOVE MEDIA   CODE INTO THE KEY               
         MVI   PPRDKRCD,X'06'      MOVE RECORD  CODE INTO THE KEY               
         OC    AGNACLI,AGNACLI     IS THERE AN OVERRIDE                         
         BZ    *+14                 NO OVERRIDE USE AGNCLI CLIENT CODE          
         MVC   PPRDKCLT,AGNACLI     ELSE MOVE IN ORIGINAL                       
         B     *+10                                                             
         MVC   PPRDKCLT,AGNCLI     MOVE CLIENT  CODE INTO THE KEY               
         MVC   PPRDKPRD,AGNPRD     MOVE PRODUCT CODE INTO THE KEY               
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),AGNSE       MOVE SE# INTO UTL+4                          
         BAS   RE,DMHIPDIR         READ HIGH FROM PRINT DIR                     
         CLC   DIR(10),DKEY        COMPARE KEY WITH ORIGINAL                    
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
*                                                                               
         BAS   RE,DMGETPRT         GET RECORD FROM PRINT FILE                   
         L     R4,AIO1                                                          
         MVC   AGNPNAM,PPRDNAME    MOVE IN CLIENT NAME                          
*                                                                               
PRPRDX   B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* FIND AGENCY NAME                                                    *         
*      R6 = CURRENT POSITION IN BINTABLE                              *         
***********************************************************************         
         SPACE 1                                                                
GETAGN   NTR1                                                                   
         LA    R1,IDTAB            R1 = A(2 AGENCY CODE/NAME TABLE)             
GETAGN10 CLI   0(R1),X'FF'         CHECK FOR END OF TABLE                       
         BNE   *+6                                                              
         DC    H'0'                CODE MUST BE IN TABLE                        
         CLC   AGNAGY,0(R1)        CHECK FOR CODE MATCH                         
         BE    *+12                  EXIT IF FOUND                              
         LA    R1,L'IDTAB(R1)      BUMP TO NEXT ENTRY                           
         B     GETAGN10                                                         
         MVC   AGNANAM,2(R1)       MOVE IN AGENCY NAME TO SORT FIELD            
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* RE-SORT TABLE BY KEY AND THEN SORT BY AGENCY ID (N,P,S)             *         
***********************************************************************         
         SPACE 1                                                                
XSRT     NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,AAGNTAB          R5 = A(TABLE)                                
         SR    R0,R0                                                            
         ICM   R0,15,BININ                                                      
         BZ    XSRTX                                                            
         USING AGND,R6                                                          
         LA    R6,BINTAB                                                        
*                                                                               
* RESORT TABLE BY KEY DUE TO OVERRIDES                                          
*                                                                               
         LA    R2,AGNLNQ           R2 = LENGTH OF RECORD                        
         LA    R3,AGNMKLQ          R3 = LENGTH OF KEY                           
         LA    R4,0                R4 = DISPLACEMENT TO KEY                     
         GOTO1 XSORT,DMCB,(0,(R6)),(R0),(R2),(R3),(R4)                          
*                                                                               
*                                                                               
* SORT BY AGENCY ID                                                             
*                                                                               
         LA    R2,AGNLNQ           R2 = LENGTH OF RECORD                        
         LA    R3,L'AGNID          R3 = LENGTH OF AGENCY ID (1 BYTE)            
         LA    R4,AGNXLNQ          R4 = DISPLACEMENT TO ID                      
         GOTO1 XSORT,DMCB,(0,(R6)),(R0),(R2),(R3),(R4)                          
*                                                                               
XSRTX    B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* FIND COSTING ACCOUNT AND UPDATE BINTABLE                            *         
***********************************************************************         
         SPACE 1                                                                
GETCST   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,AAGNTAB          R5 = A(TABLE)                                
         ICM   R0,15,BININ                                                      
         BZ    GETCSTX                                                          
         USING AGND,R6                                                          
         LA    R6,BINTAB                                                        
*                                                                               
         USING ACTRECD,R2                                                       
GETCST10 LA    R2,DKEY                                                          
         MVC   DKEY,SPACES         CLEAR DKEY                                   
         MVC   ACTKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   ACTKUNT(2),=C'SJ'   UNIT/LEDGER 'SJ'                             
         MVC   ACTKACT(3),AGNCLI   CLIENT LEVEL ONLY                            
*                                                                               
         BAS   RE,DMHIADIR         READHI ACCDIR                                
         CLC   DIR(L'DKEY),DKEY                                                 
         BE    GETCST20                                                         
         MVC   AGNERR,=C'*SJ*'     NO SJ RECORD FOUND                           
         ZAP   AGNBKT,PKZERO       ZERO OUT TOTAL                               
         B     GETCST99                                                         
*                                                                               
GETCST20 BAS   RE,DMGETACC         GETREC ACCMST                                
         L     R3,AIO1                                                          
         USING PPRELD,R3                                                        
         MVI   ELCODE,PPRELQ       X'24' - PRODUCTION PROFILE ELEMENT           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE A '24' ELEMENT                       
*                                                                               
         MVC   AGN1CKY,PPRCOST     UPDATE '1C' KEY                              
*                                                                               
         MVC   ACTKACT+3(3),AGNPRD PRODUCT LEVEL                                
         BAS   RE,DMHIADIR         READHI ACCDIR                                
         CLC   DIR(L'DKEY),DKEY                                                 
         BNE   GETCST30            NO PRODUCT LEVEL                             
         BAS   RE,DMGETACC         GETREC ACCMST                                
         L     R3,AIO1                                                          
         USING PPRELD,R3                                                        
         MVI   ELCODE,PPRELQ       X'24' - PRODUCTION PROFILE ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   GETCST30                                                         
         CLC   PPRCOST,SPACES                                                   
         BNH   GETCST30                                                         
         MVC   AGN1CKY,PPRCOST     UPDATE '1C' KEY                              
*                                                                               
GETCST30 MVC   DKEY,SPACES                                                      
         MVC   DKEY,AGN1CKY                                                     
         BAS   RE,DMHIADIR         READHI ACCDIR                                
         CLC   DIR(MINKEND),DKEY                                                
         BE    GETCST40                                                         
         MVC   AGNERR,=C'*1C*'     NO 1C RECORD FOUND                           
         ZAP   AGNBKT,PKZERO       ZERO OUT TOTAL                               
         B     GETCST99                                                         
*                                                                               
GETCST40 BAS   RE,DMGETACC         GETREC ACCMST                                
         L     R3,AIO1                                                          
         USING NAMELD,R3                                                        
         MVI   ELCODE,NAMELQ       X'20' - NAME ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   AGNCSTNM,NAMEREC                                                 
*                                                                               
         BAS   RE,GETCON           GET CONTRA ACCOUNT INFO                      
*                                                                               
GETCST99 LA    R6,AGNLNQ(R6)                                                    
         BCT   R0,GETCST10                                                      
*                                                                               
GETCSTX  B     EXIT                                                             
         EJECT                                                                  
         DROP  R2,R5                                                            
***********************************************************************         
* FIND CONTRA ACCOUNT AND UPDATE BINTABLE                             *         
***********************************************************************         
         SPACE 1                                                                
         USING AGND,R6             R6 = A(CURRENT ENTRY IN BINTABLE)            
GETCON   NTR1                                                                   
         LA    R2,DKEY                                                          
         USING MINRECD,R2                                                       
         XC    DKEY,DKEY           CLEAR DKEY                                   
         MVI   MINKTYP,MINKTYPQ    X'08' - MEDIA INTERFACE RECORD               
         MVC   MINKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   MINKMED,AGNSYSID    SYSID (N,P, OR S) + MEDIA                    
*                                                                               
GETCON20 DS    0H                                                               
         BAS   RE,DMHIADIR         READHI ACCDIR                                
         CLC   DIR(MINKEND),DKEY                                                
         BE    GETCON25                                                         
         MVC   AGNDTMED(9),=C'NOT FOUND'    MARK RECORD AS 'NOT FOUND'          
         MVC   AGNERR,=C'*MI*'                                                  
         B     GETCONX                                                          
*                                                                               
GETCON25 BAS   RE,DMGETACC         GETREC ACCMST                                
         L     R3,AIO1                                                          
         USING MDIELD,R3                                                        
         MVI   ELCODE,MDIELQ       X'19' - MEDIA INTERFACE ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                ELEMENT HAS TO BE THERE                      
*                                                                               
         MVC   AGNDTMED,MDIDESC    DESCRIPTION                                  
         MVC   AGN11CPY,RCCOMPFL                                                
         MVC   AGN11UL,=C'11'                                                   
         MVC   AGN11REC,MDICOST    COST ANALSYS ('11' ACCOUNT)                  
*                                                                               
         LA    R2,DKEY             GET CONTRA ACCOUNT NAME                      
         USING ACTRECD,R2                                                       
         XC    DKEY,DKEY                                                        
         MVC   ACTKCULA,AGN11KY                                                 
         BAS   RE,DMHIADIR         READHI ACCDIR                                
         CLC   DIR(L'AGN11KY),DKEY                                              
         BE    *+14                                                             
         MVC   AGNERR,=C'*11*'                                                  
         B     GETCONX                                                          
*                                                                               
         BAS   RE,DMGETACC         GETREC ACCMST                                
         L     R3,AIO1                                                          
         USING NAMELD,R3                                                        
         MVI   ELCODE,NAMELQ       X'20' - NAME ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                ELEMENT HAS TO BE THERE                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   AGNCONNM(0),NAMEREC                                              
*                                                                               
GETCONX  B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* CONVERT AGENCY TABLE INTO SJ TABLE FOR UPDATING                     *         
***********************************************************************         
         SPACE 1                                                                
CONTAB   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,AAGNTAB          R5 = A(TABLE)                                
         ICM   R0,15,BININ                                                      
         BZ    GETCSTX                                                          
         USING AGND,R6                                                          
         LA    R6,BINTAB                                                        
         USING SJRECD,R5                                                        
         LA    R5,SJWRK            R5 = A(BINTABLE WORK AREA)                   
*                                                                               
CONTAB10 CLC   AGNCLI,=C'***'      FILTER OUT NON ZENITH CLIENTS                
         BE    CONTAB99                                                         
         CLC   AGNAGY,=C'CE'       ONLY OFFICE 'P' FOR CME                      
         BNE   *+12                                                             
         CLI   AGNOFF,C'P'                                                      
         BNE   CONTAB99                                                         
         OC    AGNERR,AGNERR       SKIP IF ERRORS                               
         BNZ   CONTAB99                                                         
*                                                                               
         XC    SJWRK,SJWRK                                                      
         MVC   SJ1CKY,AGN1CKY      COSTING ACCOUNT KEY                          
         MVC   SJ11KY,AGN11KY      CONTRA ACCOUNT KEY                           
         MVC   SJMOS,AGNMTHPK      BILLING MONTH PACKED                         
         MVC   SJMTH,AGNMTH        BILLING MONTH                                
         MVC   SJCONNM,AGNCONNM    CONTRA ACCOUNT NAME                          
         MVC   SJCSTNM,AGNCSTNM    COSTING ACCOUNT NAME                         
         ZAP   SJBKT,AGNBKT                                                     
         GOTO1 BINADD,DMCB,SJWRK,ASJTAB                                         
*                                                                               
CONTAB99 LA    R6,AGNLNQ(R6)                                                    
         BCT   R0,CONTAB10                                                      
*                                                                               
CONTABX  B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE BUCKETS VIA ADDTRANS                                         *         
***********************************************************************         
         SPACE 1                                                                
UPDATE   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,ASJTAB           R5 = A(TABLE)                                
         ICM   R2,15,BININ                                                      
         BZ    PRTRTEX                                                          
         USING SJRECD,R6                                                        
         LA    R6,BINTAB                                                        
*                                                                               
UPDATE10 L     RE,ATRNBLOK         CLEAR ADDTRN BLOCK                           
         LA    RF,TRNBLKL                                                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TRNBLKD,R3                                                       
         L     R3,ATRNBLOK                                                      
         USING CPYELD,RF                                                        
         L     RF,ADCMPEL                                                       
         MVC   TRNCOMF,ADCOMFAC                                                 
         MVC   TRNCPYS1,CPYSTAT1   COMPANY STATUS BYTES                         
         MVC   TRNCPYS2,CPYSTAT2                                                
         MVC   TRNCPYS3,CPYSTAT3                                                
         MVC   TRNCPYS4,CPYSTAT4                                                
         MVC   TRNCPYS5,CPYSTAT5                                                
         MVC   TRNCPYS6,CPYSTAT6                                                
         MVC   TRNCPYS7,CPYSTAT7                                                
         MVC   TRNCPYS8,CPYSTAT8                                                
         CLI   CPYLN,CPYLN3Q       LONG ENOUGH FOR THESE FIELDS?                
         BL    *+16                NO                                           
         MVC   TRNCPYS9,CPYSTAT9                                                
         MVC   TRNCPYSA,CPYSTATA                                                
*                                                                               
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
*                                                                               
         DROP  RF                                                               
         MVC   TRNREC,AIO1         AIO1 = A(TRANSACTION RECORD)                 
         MVC   TRNBMOS,SJMOS       MONTH OF SERVICE FOR POSTINGS                
         OI    TRNMODE,TRNMOFLN+TRNMEMUY     OFFLINE AND NEW FILE               
         OI    TRNINDS2,TRNIUBKO+TRNIRBKA    REPLACE BUCKET AMOUNTS             
         OI    TRNINDS,TRNICONV    PASSING ADDTRN NEW FILE FORMAT               
         CLI   RCWRITE,C'N'                                                     
         BNE   *+8                                                              
         OI    TRNINDS,TRNIWRNO    WRITE=NO                                     
         GOTO1 DATCON,DMCB,(1,TODAYP),(2,TRNEFDT)     EFFECTIVE DATE            
         DROP  R3                                                               
*                                                                               
*              DR 1C - C/A 11                                                   
*                                                                               
         USING TRNELD,R4                                                        
         LA    R4,ELEM44           *** TRANSACTION ELEMENT - X'44' ***          
         XC    ELEM44,ELEM44                                                    
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVC   TRNDATE,TODAYP                                                   
         MVC   TRNREF,=C'ZENITH'                                                
         MVI   TRNTYPE,9                                                        
         MVC   TRNANAL,SPACES                                                   
         ZAP   TRNAMNT,PKZERO                                                   
         OI    TRNSTAT,TRNSDR      DEBIT                                        
         MVC   TRNMOS,SJMOS        CONVERT YYMM -> YM EDCDIC                    
         OI    TRNMOS,X'F0'        EX/  X'9402' -> C'42'                        
         SR    R1,R1                                                            
         IC    R1,TRNMOS+1                                                      
         LA    RF,X'F0'                                                         
         TM    TRNMOS+1,X'10'                                                   
         BNO   *+8                                                              
         LA    RF,X'B1'                                                         
         AR    R1,RF                                                            
         STC   R1,TRNMOS+1                                                      
         MVC   TRNBREF,=C'ACMB'                                                 
         DROP  R4                                                               
*                                                                               
         USING SCIELD,R4           *** BUCKETS ELEMENT - X'50' ***              
         LA    R4,ELEM50                                                        
         XC    ELEM50,ELEM50                                                    
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITNULL                                                 
         ZAP   SCIAMNT,SJBKT                                                    
         ZAP   SCIADMN,PKZERO                                                   
         MVC   SCISUBTY,SPACES                                                  
         DROP  R4                                                               
*                                                                               
         L     RE,AIO1             CLEAR IO AREA                                
         LH    RF,=Y(MXRLNQ)                                                    
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TRNRECD,R5          BUILD TRANSACTION RECORD                     
         L     R5,AIO1                                                          
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,SJ1CKY     C/U/L/ACCOUNT                                
         MVC   TRNKOFF,SJ1COFF     OFFICE CODE                                  
         MVC   TRNKCULC,SJ11KY     C/U/L/CONTRA-ACCOUNT                         
         MVC   TRNKDATE,TODAYP                                                  
         MVC   TRNKREF,=C'ZENITH'                                               
         XC    TRNKSBR,TRNKSBR                                                  
         LH    RE,=Y(TRNRFST-TRNRECD+1)                                         
         STCM  RE,3,TRNRLEN        RECORD LENGTH                                
         GOTO1 HELLO,DMCB,(C'P',ACCMST),TRNRECD,ELEM44,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         GOTO1 HELLO,DMCB,(C'P',ACCMST),TRNRECD,ELEM50,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         DROP  R5                                                               
*                                                                               
         USING TRNBLKD,R3                                                       
         L     R3,ATRNBLOK                                                      
         MVC   TRNCACNM,SJCONNM                                                 
         BAS   RE,DUMP                                                          
         GOTO1 ADDTRN,TRNBLKD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
*              CR 11 - C/A 1C                                                   
*                                                                               
         L     RE,AIO1                                                          
         LH    RF,=Y(MXRLNQ)                                                    
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TRNELD,R4                                                        
         LA    R4,ELEM44           *** TRANSACTION ELEMENT - X'44' ***          
         MVI   TRNSTAT,0           CREDIT                                       
         DROP  R4                                                               
*                                                                               
         USING SCIELD,R4           *** BUCKETS ELEMENT - X'50' ***              
         LA    R4,ELEM50                                                        
         ZAP   SCIAMNT,PKZERO                                                   
         ZAP   SCIADMN,SJBKT                                                    
         DROP  R4                                                               
*                                                                               
         USING TRNRECD,R5          BUILD TRANSACTION RECORD                     
         L     R5,AIO1                                                          
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,SJ11KY     C/U/L/ACCOUNT                                
         MVC   TRNKOFF,SJ1COFF     OFFICE CODE                                  
         MVC   TRNKCULC,SJ1CKY     C/U/L/CONTRA-ACCOUNT                         
         MVC   TRNKDATE,TODAYP                                                  
         MVC   TRNKREF,=C'ZENITH'                                               
         XC    TRNKSBR,TRNKSBR                                                  
         LH    RE,=Y(TRNRFST-TRNRECD+1)                                         
         STCM  RE,3,TRNRLEN        RECORD LENGTH                                
         GOTO1 HELLO,DMCB,(C'P',ACCMST),TRNRECD,ELEM44,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         GOTO1 HELLO,DMCB,(C'P',ACCMST),TRNRECD,ELEM50,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         DROP  R5                                                               
*                                                                               
         USING TRNBLKD,R3                                                       
         L     R3,ATRNBLOK                                                      
         MVC   TRNCACNM,SJCSTNM                                                 
         BAS   RE,DUMP                                                          
         GOTO1 ADDTRN,TRNBLKD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         LA    R6,SJLNQ(R6)                                                     
         BCT   R2,UPDATE10                                                      
*                                                                               
UPDATEX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT LIST OF RECORDS (NUMBER OF OCCURENCES)                        *         
***********************************************************************         
         SPACE 1                                                                
PRTRTE   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,AAGNTAB          R5 = A(TABLE)                                
         ICM   R0,15,BININ                                                      
         BZ    PRTRTEX                                                          
         USING AGND,R6                                                          
         LA    R6,BINTAB                                                        
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         MVI   RCSUBPRG,0          SET FOR PRINTOUT 1                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   PKPRDCNT,PKZERO     PRODUCT COUNTER                              
         ZAP   PKCLITOT,PKZERO     CLIENT  TOTAL                                
         ZAP   PKPRDTOT,PKZERO     PRODUCT TOTAL                                
         ZAP   PKAGYTOT,PKZERO     AGENCY  TOTAL                                
         XC    LSTBINKY,LSTBINKY   CLEAR LAST CLI/PROD/AGY FIELD                
         XC    FLAG,FLAG                                                        
         MVC   PRTLNE(PRLNQ),XSPACES CLEAR PRINT LINE WITH SPACES               
         MVC   AGYNAME,SPACES                                                   
         MVC   AGYNAME(2),AGNAGY       MOVE AGENCY CODE INTO HEADLINES          
         MVI   AGYNAME+3,C'-'          SEPARATE CODE FROM NAME W/'-'            
         MVC   AGYNAME+5(36),AGNANAM   MOVE AGENCY NAME INTO HEADLINES          
*                                                                               
PRTRTE10 CLC   AGNCLI,=C'***'      IF CLIENT CODE IS *** SKIP                   
         BE    PRTRTE99                                                         
         CLC   AGNAGY,=C'CE'       ONLY OFFICE 'P' FOR CAMBELL                  
         BNE   *+12                                                             
         CLI   AGNOFF,C'P'                                                      
         BNE   PRTRTE99                                                         
*                                                                               
         OC    LSTBINKY,LSTBINKY                                                
         BZ    PRTRTE20                                                         
         CLC   LSTAGY,AGNAGY       SET FLAG FOR CHANGES                         
         BE    *+8                                                              
         OI    FLAG,FLGAGY         AGENCY FLAG                                  
         CLC   LSTCLI,AGNCLI                                                    
         BE    *+8                                                              
         OI    FLAG,FLGCLI         CLIENT FLAG                                  
         CLC   LSTPRD,AGNPRD                                                    
         BE    *+8                                                              
         OI    FLAG,FLGPRD         PRODUCT FLAG                                 
         CLC   LSTSM,AGNSYSID                                                   
         BE    *+8                                                              
         OI    FLAG,FLGSM          SYS/MED FLAG                                 
         CLC   LSTDTE,AGNMTHPK                                                  
         BE    *+8                                                              
         OI    FLAG,FLGDTE         DATE FLAG                                    
*                                                                               
         TM    FLAG,FLGPRD                                                      
         BNO   *+8                                                              
         BAS   RE,PRDTOT           PRODUCT TOTAL ROUTINE                        
         TM    FLAG,FLGCLI                                                      
         BNO   *+8                                                              
         BAS   RE,CLITOT           CLIENT TOTAL ROUTINE                         
         TM    FLAG,FLGAGY                                                      
         BNO   *+8                                                              
         BAS   RE,AGYTOT                                                        
*                                                                               
PRTRTE20 CLC   LSTAGY,AGNAGY                                                    
         BE    PRTRTE30                                                         
         MVC   AGYNAME(2),AGNAGY   MOVE NEW AGENCY CODE INTO HEADLINES          
         MVI   AGYNAME+3,C'-'          SEPARATE CODE FROM NAME W/'-'            
         MVC   AGYNAME+5(36),AGNANAM   MOVE AGENCY NAME INTO HEADLINES          
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PRTRTE30 CLC   LSTCLI,AGNCLI                                                    
         BE    PRTRTE40                                                         
         MVC   PCLT,AGNCLI         MOVE IN AGENCY CLIENT CODE                   
         MVC   PCPANAM(20),AGNCNAM MOVE IN CLIENT NAME                          
         LA    R2,PUNDLNQ          R2 = UNDERLINE LENGTH                        
         GOTO1 UNDERLIN,DMCB,((R2),PCLT),(X'BF',PCLT+L'XP)                      
         GOTO1 ACREPORT                                                         
         B     PRTRTE60                                                         
*                                                                               
PRTRTE40 CLC   LSTPRD,AGNPRD                                                    
         BNE   PRTRTE60                                                         
         CLC   LSTSM,AGNSYSID                                                   
         BNE   PRTRTE70                                                         
         CLC   LSTDTE,AGNMTHPK                                                  
         BNE   PRTRTE80                                                         
*                                                                               
PRTRTE60 MVC   PPRD,AGNPRD                                                      
         MVC   PCPANAM(20),AGNPNAM MOVE IN PRODUCT NAME                         
PRTRTE70 MVC   PSYSMD,AGNSYSID                                                  
         MVC   PMED,AGNDTMED       MOVE IN DETAILED MEDIA                       
PRTRTE80 MVC   PMNTH,AGNMTH        MONTH                                        
         EDIT  (P8,AGNBKT),(17,PBAMNT),2,COMMAS=YES,FLOAT=$,MINUS=YES           
         AP    PKCLITOT,AGNBKT     ADD TO CLIENT  TOTAL                         
         AP    PKPRDTOT,AGNBKT     ADD TO PRODUCT TOTAL                         
         AP    PKAGYTOT,AGNBKT     ADD TO AGENCY  TOTAL                         
         AP    PKPRDCNT,=P'1'      ADD ONE TO COUNTER                           
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   LSTBINKY,0(R6)      UPDATE LAST CLI/PROD/AGY FIELD               
         XC    FLAG,FLAG                                                        
PRTRTE99 LA    R6,AGNLNQ(R6)                                                    
         BCT   R0,PRTRTE10                                                      
*                                                                               
         BAS   RE,PRDTOT                                                        
         BAS   RE,CLITOT                                                        
         BAS   RE,AGYTOT                                                        
*                                                                               
PRTRTEX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT PRODUCT TOTALS ROUTINE                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING AGND,R6                                                          
PRDTOT   NTR1                                                                   
         CP    PKPRDCNT,=P'1'                                                   
         BNH   PRDTOTX                                                          
         MVC   PPRD(17),=C'TOTAL FOR PRODUCT'                                   
         MVC   PPRD+18,LSTPRD                LAST PRODUCT CODE                  
         EDIT  (P8,PKPRDTOT),(17,PBAMNT),2,COMMAS=YES,FLOAT=$,MINUS=YES         
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRDTOTX  ZAP   PKPRDTOT,PKZERO                                                  
         ZAP   PKPRDCNT,PKZERO                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT CLIENT  TOTALS ROUTINE                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING AGND,R6                                                          
CLITOT   NTR1                                                                   
         MVC   PPRD(17),=C'TOTAL FOR CLIENT '                                   
         MVC   PPRD+18,LSTCLI         LAST CLIENT CODE                          
         EDIT  (P8,PKCLITOT),(17,PBAMNT),2,COMMAS=YES,FLOAT=$,MINUS=YES         
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         ZAP   PKCLITOT,PKZERO     RE-INITIALIZE FIELD                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT AGENCY  TOTALS ROUTINE                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING AGND,R6                                                          
AGYTOT   NTR1                                                                   
         MVC   PCLT(7),=C'*TOTAL*'                                              
         MVC   PCPANAM,AGYNAME     LAST AGENCY NAME                             
         EDIT  (P8,PKAGYTOT),(17,PBAMNT),2,COMMAS=YES,FLOAT=$,MINUS=YES         
         GOTO1 ACREPORT                                                         
         ZAP   PKAGYTOT,PKZERO     RE-INITIALIZE FIELD                          
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT LIST OF 1C/11 AND TOTALS                                      *         
***********************************************************************         
         SPACE 1                                                                
PRTTAB   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,ASJTAB           R5 = A(TABLE)                                
         ICM   R0,15,BININ                                                      
         BZ    PRTRTEX                                                          
         USING SJRECD,R6                                                        
         LA    R6,BINTAB                                                        
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         XC    LSTSJKEY,LSTSJKEY                                                
         XC    FLAG,FLAG                                                        
         ZAP   PK1CTOT,PKZERO                                                   
         ZAP   PK1CCNT,PKZERO                                                   
         ZAP   PK11TOT,PKZERO                                                   
         ZAP   PK11CNT,PKZERO                                                   
*                                                                               
         MVI   RCSUBPRG,1          SET FOR PRINTOUT 2                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     PRTTAB10            1ST TIME THROUGH - SKIP COMPARES             
*                                                                               
PRTTAB05 CLC   LST1CKY,SJ1CKY                                                   
         BE    *+12                                                             
         OI    FLAG,FLG1C          MARK BOTH 1C AND 11 AS CHANGED               
         OI    FLAG,FLG11                                                       
         CLC   LST11KY,SJ11KY                                                   
         BE    *+8                                                              
         OI    FLAG,FLG11          MARK AS CHANGED                              
*                                                                               
         TM    FLAG,FLG11                                                       
         BNO   *+8                                                              
         BAS   RE,TOT11                                                         
         TM    FLAG,FLG1C                                                       
         BNO   *+8                                                              
         BAS   RE,TOT1C                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTTAB10 CLC   LST1CKY,SJ1CKY      SAME COSTING ACCOUNT KEY??                   
         BNE   PRTTAB20                                                         
         CLC   LST11KY,SJ11KY      SAME ANALYSIS ACCOUNT                        
         BNE   *+14                                                             
         B     PRTTAB30                                                         
*                                                                               
PRTTAB20 MVC   P21CKY,SJ1CKY+1     NEW COSTING ACCOUNT KEY                      
         MVC   P211KY,SJ11KY+1     CONTRA ACCOUNT KEY                           
PRTTAB30 MVC   P2MOS,SJMTH         BILLING MONTH                                
         EDIT  (P8,SJBKT),(17,P2BLAMT),2,MINUS=YES,FLOAT=$                      
         AP    PK1CTOT,SJBKT                                                    
         AP    PK11TOT,SJBKT                                                    
         AP    PK1CCNT,=P'1'                                                    
         AP    PK11CNT,=P'1'                                                    
*                                                                               
         MVC   LSTSJKEY,0(R6)                                                   
         XC    FLAG,FLAG                                                        
         LA    R6,SJLNQ(R6)                                                     
         BCT   R0,PRTTAB05                                                      
*                                                                               
         BAS   RE,TOT11            PRINT TOTAL OF ANALYSIS ACCOUNT              
         BAS   RE,TOT1C              AND COSTING ACCOUNT                        
*                                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT COSTING ACCOUNT TOTALS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING AGND,R6                                                          
TOT1C    NTR1                                                                   
         CP    PK1CCNT,=P'1'                                                    
         BNH   TOT1CX              IF NOT MORE THAN 1 OCCURANCE - EXIT          
         GOTO1 ACREPORT                                                         
         MVC   P2BLAMT+7(9),=C'**TOTAL**'                                       
         EDIT  (P8,PK1CTOT),(17,P21CTOT),2,MINUS=YES,FLOAT=$                    
         GOTO1 ACREPORT                                                         
*                                                                               
TOT1CX   ZAP   PK1CCNT,PKZERO                                                   
         ZAP   PK1CTOT,PKZERO                                                   
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT ANALYSIS ACCOUNT TOTALS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING AGND,R6                                                          
TOT11    NTR1                                                                   
         CP    PK11CNT,=P'1'                                                    
         BNH   TOT11X              IF NOT MORE THAN 1 OCCURANCE - EXIT          
         EDIT  (P8,PK11TOT),(17,P21CTOT),2,MINUS=YES,FLOAT=$                    
*                                                                               
TOT11X   ZAP   PK11CNT,PKZERO                                                   
         ZAP   PK11TOT,PKZERO                                                   
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         ICM   R6,1,BINFST         DISPLACEMENT TO BUCKETS                      
         BZ    BINXIT                NO BUCKETS - EXIT                          
         LR    R5,R4               A(RECORD FOUND)                              
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
         AP    0(8,R4),0(8,R3)     ADD TO BUCKET                                
*                                                                               
BINXIT   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R2                                                       
DMHIADIR NTR1                      DMRDHI FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCDIR',DKEY,DIR,0                
         LA    R2,DIR                                                           
         MVC   DA,ACCKDA           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSEADIR NTR1                      DMRSEQ FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'ACCDIR',DKEY,DIR,0                
         LA    R2,DIR                                                           
         MVC   DA,ACCKDA           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
*                                                                               
DMGETACC NTR1                      GETREC FOR SPOT FILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'ACCMST',DA,AIO1,DMWORK            
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMHISDIR NTR1                      DMRDHI FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'SPTDIR',DKEY,DIR,0                
         MVC   DA,DIR+14           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSESDIR NTR1                      DMRSEQ FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'SPTDIR',DKEY,DIR,0                
         MVC   DA,DIR+14           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETSPT NTR1                      GETREC FOR SPOT FILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'SPTFILE',DA,AIO1,DMWORK           
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMHIPDIR NTR1                      DMRDHI FOR PRINT DIR                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',DKEY,DIR,0                
         MVC   DA,DIR+27           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSEPDIR NTR1                      DMRSEQ FOR PRINT DIR                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PRTDIR',DKEY,DIR,0                
         MVC   DA,DIR+27           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETPRT NTR1                      GETREC FOR PRINT FILE                        
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PRTFILE',DA,AIO1,DMWORK           
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMCHK    NTR1                      CHECK RETURNS ON DATAMGR CALLS               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
         CP    MAXDMP,DMPTOT       CHECK IF MAXIMUM WAS REACHED                 
         BNH   DUMPX                                                            
         AP    DMPTOT,=P'1'                                                     
         LA    R0,L'MSG1                                                        
         LA    R2,MSG1                                                          
         L     R3,AIO1                                                          
         SR    R4,R4                                                            
         ICM   R4,3,ACCRLEN-ACCRECD(R3)                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GETEL DECLARATION                                                   *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R3,56,ELCODE                                                     
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BNE   BXH10                                                            
*                                                                               
         MVC   XHEAD2+1(L'AGYNAME),AGYNAME   AGENCY NAME                        
         MVC   XHEAD3+1(L'PRTDFT),PRTDFT     DRAFT ONLY??                       
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(PSYSMD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PMED-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PMNTH-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(PBAMNT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PRTERRS-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXH10    CLI   RCSUBPRG,1                                                       
         BNE   BXXIT                                                            
*                                                                               
         MVC   XHEAD2+1(L'PRTDFT),PRTDFT     DRAFT ONLY??                       
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(P211KY-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(P2MOS-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(P2BLAMT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(P21CTOT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+PRLNQ2,C'R'                                              
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
UTL      DS    0D                                                               
         DC    F'0',X'02'                                                       
         DC    XL3'00'                                                          
*                                                                               
MXRLNQ   EQU   2000                MAX RECORD SIZE                              
MSG1     DC    C'ACCOUNT RECORD'                                                
*                                                                               
OPTN     DC    X'00'               RUN OPTIONS                                  
OPTDFT   EQU   X'80'               RUN PRINTOUT ONLY                            
OPTAGY   EQU   X'40'               PRINT SELECTED AGENCY ONLY                   
OPTSYS   EQU   X'20'               RUN ON SELECTED FILES ONLY (N,P,S)           
*                                                                               
DMPTOT   DC    PL4'0'              DUMP COUNT                                   
MAXDMP   DC    PL4'15'             MAXIMUM DUMPS                                
*                                                                               
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
CLUNPK   DC    V(CLUNPK)                                                        
HELLO    DC    V(HELLO)                                                         
HELEN    DC    V(HELEN)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
UNDERLIN DC    V(UNDERLIN)                                                      
ADDTRN   DC    V(ADDTRN)                                                        
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
AIO1     DC    A(IO1)                                                           
AAGNTAB  DC    A(AGNTAB)                                                        
ASJTAB   DC    A(SJTAB)                                                         
ATRNBLOK DC    A(TRNSBLOK)                                                      
*                                                                               
SPFLIST  DS    0H                  SPOT FILE LIST                               
         DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    C'X'                                                             
*                                                                               
PRFLIST  DS    0H                  PRINT FILE LIST                              
         DC    CL8'NPRTDIR'                                                     
         DC    CL8'NPRTFILE'                                                    
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
MEDTAB   DS    0CL1                TABLE OF MEDIA CODES FOR PRINT               
         DC    C'M'                                                             
         DC    C'N'                                                             
         DC    C'O'                                                             
         DC    C'S'                                                             
         DC    C'T'                                                             
         DC    X'FF'                                                            
*                                                                               
SYSTAB   DS    0F                  SYSTEM FILES                                 
*        DC    X'10',X'03',C'CE',C'SC',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'80',X'03',C'DT',C'SD',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'40',X'03',C'SF',C'SD',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'B0',X'07',C'DF',C'SD',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'20',X'22',C'BS',C'SB',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'20',X'23',C'BT',C'NB',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'80',X'23',C'CE',C'NC',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'50',X'25',C'DF',C'ND',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'C2',X'04',C'CE',C'PC',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'C4',X'04',C'DF',C'PD',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'C5',X'04',C'DT',C'PD',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'E2',X'04',C'SF',C'PD',C'PRINT   ',AL4(PRFLIST,PRTRD)           
*        DC    X'C9',X'54',C'BS',C'PB',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'FF'                                                            
*                                                                               
IDTAB    DS    0CL38                                                            
         DC    C'BS',C'BATES USA                           '                    
         DC    C'BT',C'BACKER SPIELVOGEL BATES INC.        '                    
         DC    C'CE',C'CAMBELL - MITHUN - ESTY             '                    
         DC    C'DF',C'SAATCHI AND SAATCHI ADVERTISING INC.'                    
         DC    C'DT',C'SAATCHI AND SAATCHI DFS INC.        '                    
         DC    C'SF',C'SAATCHI AND SAATCHI DFS INC         '                    
         DC    X'FF'                                                            
*                                                                               
SPTTAB   DS    0CL2                                                             
         DC    X'01',C'T'                                                       
         DC    X'02',C'R'                                                       
         DC    X'03',C'N'                                                       
         DC    X'04',C'X'                                                       
         DC    X'08',C'C'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
         DC    C'**IO1***'                                                      
RLN1     DC    F'0'                      IOAREA #1                              
IO1      DC    (MXRLNQ)X'00'                                                    
         DC    C'**TBLK**'                                                      
TRNSBLOK DS    0D                                                               
         DS    CL(TRNBLKL)                                                      
*                                                                               
*        BINTABLE CONSTANTS                                                     
*                                                                               
AGNTAB   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(AGNLNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(AGNKLNQ)        KEY LENGTH                                   
         DC    AL4(AGNMAX)         MAX IN TABLE                                 
         DC    AL1(AGNBKCT)        NUMBER OF BUCKETS                            
         DC    AL1(AGNBKT-AGND)    DISPLACEMENT TO FIRST BUCKET                 
         DS    (AGNMAX*AGNLNQ)XL1  TABLE                                        
*                                                                               
SJTAB    DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(SJLNQ)          LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(SJKLNQ)         KEY LENGTH                                   
         DC    AL4(SJMAX)          MAX IN TABLE                                 
         DC    AL1(SJBKCT)         NUMBER OF BUCKETS                            
         DC    AL1(SJBKT-SJRECD)   DISPLACEMENT TO FIRST BUCKET                 
         DS    (SJMAX*SJLNQ)XL1    TABLE                                        
*                                                                               
SJMAX    EQU   5000                                                             
AGNMAX   EQU   5000                                                             
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
ACMBD    DSECT                                                                  
AUTL     DS    A                                                                
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
DA       DS    F                   DISK ADDRESS                                 
DIR      DS    CL64                DIRECTORY RECORD                             
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
SAVEKEY  DS    CL(L'ACCKEY)        SAVED AREA FOR KEY                           
AGYNAME  DS    CL41                SAVED AREA FOR AGENCY NAME                   
*                                                                               
LSTBINKY DS    0CL10               LAST CLIENT/PRODUCT/AGENCY                   
LSTAGY   DS    CL2                   LAST AGENCY                                
LSTCLI   DS    CL3                   LAST CLIENT                                
LSTPRD   DS    CL3                   LAST PRODUCT                               
LSTSM    DS    CL2                   LAST SYSTEM/MEDIA                          
LSTDTE   DS    CL2                   LAST DATE                                  
*                                                                               
LSTSJKEY DS    0CL(L'SJ1CKY+L'SJ11KY)  LAST SJ KEY (1C/11 KEYS)                 
LST1CKY  DS    CL(L'SJ1CKY)              LAST COSTING ACCOUNT KEY               
LST11KY  DS    CL(L'SJ11KY)              LAST CONTRA ACCOUNT KEY                
*                                                                               
PRTDFT   DS    CL10                SAVED AREA FOR PRINTOUT IF DRAFT             
MOSDTE   DS    CL6                 MONTH OF SERVICE DATE                        
SAVEID   DS    CL2                 SAVE AREA FOR COMPANY ID                     
SYSSAVE  DS    CL1                 SAVED AREA FOR SYSTEM RUN REQUEST ID         
SVSYSID  DS    CL1                 SAVED AREA FOR SYSTEM ID                     
SVAGY    DS    CL1                 SAVED AREA FOR AGENCY ID                     
*                                                                               
FLAG     DS    XL1                 FLAG FOR CHANGES                             
FLGAGY   EQU   X'80'               AGENCY  CHANGED FLAG                         
FLGCLI   EQU   X'40'               CLIENT  CHANGED FLAG                         
FLGPRD   EQU   X'20'               PRODUCT CHANGED FLAG                         
FLGSM    EQU   X'10'               SYS/MED CHANGED FLAG                         
FLGDTE   EQU   X'08'               DATE    CHANGED FLAG                         
FLG1C    EQU   X'04'               COSTING CHANGED FLAG                         
FLG11    EQU   X'02'               CONTRA  CHANGED FLAG                         
TURNOFF  EQU   X'FF'               TURN OFF                                     
SESAVE   DS    XL1                 SE NUMBER SAVE AREA                          
SVSENUM  DS    XL1                 SAVE AREA FOR SYSTEM SE NUMBER               
SAVEAM   DS    XL1                 SAVED AREA FOR AGENCY/MEDIA                  
SVMED    DS    XL1                 SAVED AREA FOR SPOT MEDIA                    
SAVEYR   DS    XL1                 SAVED AREA FOR YEAR                          
*                                                                               
PKCLITOT DS    PL8                 PACKED TOTAL BY CLIENT                       
PKPRDTOT DS    PL8                 PACKED TOTAL BY PRODUCT                      
PKAGYTOT DS    0PL8                PACKED TOTAL BY AGENCY FOR ROUTE 1           
PK1CTOT  DS    PL8                 PACKED TOTAL BY 1C FOR ROUTINE 2             
PK11TOT  DS    PL8                 PACKED TOTAL BY CONTRA ACCOUNT               
PKPRDCNT DS    0PL8                COUNTER FOR PRODUCTS (ROUTINE 1)             
PK1CCNT  DS    PL8                 COUNTER FOR COSTING ACCT (ROUTINE 2)         
PK11CNT  DS    PL8                 COUNTER FOR CONTRA ACCOUNT                   
*                                                                               
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
PKDTE    DS    0PL3                FULL BILLING START DATE                      
PKBILDTE DS    PL2                 REAL BILLING START DATE                      
         DS    PL1                 SPARE                                        
PKZERO   DS    PL1                 ZERO                                         
*                                                                               
AGNWRK   DS    CL(AGNLNQ)          WORK AREA FOR AGENCY ENTRY                   
SJWRK    DS    CL(SJLNQ)           WORK AREA FOR SJ/1C/11 ENTRY                 
*                                                                               
ELCODE   DS    XL1                 ELEMENT CODE FOR GETEL                       
ELEM44   DS    XL255               COMPOSITE ELEMENT '44'                       
ELEM50   DS    XL255               COMPOSITE ELEMENT '50'                       
*                                                                               
PALAREA  DS    XL20                P&L BUCKET AREA                              
         EJECT                                                                  
***********************************************************************         
* SYSTEM TABLE FILE DSECT                                             *         
***********************************************************************         
         SPACE 1                                                                
SYSTBLD  DSECT                                                                  
SYSTMED  DS    XL1                 COMPANY AGENCY/MEDIA                         
SYSTSE   DS    XL1                 AGENCY SE NUMBER                             
SYSTCODE DS    CL2                 AGENCY CHARACTER CODE                        
SYSTID   DS    CL1                 ONE BYTE SYSTEM IDENTIFICATION               
SYSTAGY  DS    CL1                 ONE BYTE AGENCY IDENTIFICATION               
SYSTNAM  DS    CL8                 SYSTEM NAME (SPOT, NET, PRINT)               
ASYSFIL  DS    AL4                 A(SYSTEM FILES TO BE OPEN)                   
ASYSRD   DS    AL4                 A(SYSTEM READ ROUTINES)                      
SYSTLNQ  EQU   *-SYSTBLD           LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN CLIENT/PRODUCT TABLE 1                           *         
***********************************************************************         
         SPACE 1                                                                
AGND     DSECT                                                                  
AGNAGY   DS    CL2             SOURCE AGENCY CODE                               
AGNCLI   DS    CL3             CLIENT CODE                                      
AGNPRD   DS    CL3             PRODUCT CODE                                     
AGNMKLQ  EQU   *-AGND          LENGTH OF MINI-KEY                               
*                                                                               
* KEEP ID AND MEDIA BACK TO BACK USED FOR MI RECORDS AS A 2 BYTE FIELD          
AGNSYSID DS    CL1             ONE BYTE SYSTEM ID (N,P,S)                       
AGNMED   DS    CL1             CHARACTER MEDIA                                  
AGNMTHPK DS    CL2             BILLING MONTH IN PACKED FORMAT                   
AGNKLNQ  EQU   *-AGND          LENGTH OF KEY                                    
AGNBKT   DS    PL8                 BUCKET                                       
AGNBKLN  EQU   *-AGNBKT            BUCKET LENGTH                                
AGNBKCT  EQU   (*-AGNBKT)/AGNBKLN  NUMBER OF BUCKETS                            
AGNXLNQ  EQU   *-AGND              DISPLACEMENT TO ID FOR XSORT                 
AGNID    DS    CL1             AGENCY ID(B-BATES,C-CAMPBELL,D-SAATCHI)          
AGNACLI  DS    CL3             ORIGINAL CLIENT CODE IF OVERRIDE EXISTS          
AGNPKCLI DS    CL2             AGENCY CLIENT CODE PACKED (SPOT)                 
AGNOFF   DS    CL1             OFFICE CODE                                      
AGNSW    DS    XL1             SWITCH TO TELL IF ENTRY WAS CONVERTED            
AGNSYS   DS    XL1             X'80' - SPOT / X'00' - PRINT                     
AGNSE    DS    XL1             SE NUMBER                                        
AGNSPTAM DS    CL1             ORIGINAL SPOT AGENCY/MEDIA                       
AGNMTH   DS    CL6             BILLING MONTH                                    
AGNERR   DS    CL4             ERRORS WHILE CREATING ACCOUNTS                   
AGNDTMED DS    CL12            MEDIA DETAILED                                   
AGNANAM  DS    CL36            AGENCY NAME                                      
AGNCNAM  DS    CL20            CLIENT NAME                                      
AGNPNAM  DS    CL20            PRODUCT NAME                                     
AGN1CKY  DS    0CL15           COSTING ACCOUNT KEY                              
AGN1CCPY DS    XL1               COSTING COMPANY                                
AGN1CUL  DS    CL2               UNIT/LEDGER '1C'                               
AGN1CTYP DS    CL1               TYPE                                           
AGN1COFF DS    CL2               OFFICE                                         
AGN1CCLI DS    CL3               CLIENT CODE                                    
AGN1CPRD DS    CL6               PRODUCT CODE                                   
AGNCSTNM DS    CL20              COSTING ACCOUNT NAME                           
AGN11KY  DS    0CL15           CONTRA  ACCOUNT KEY                              
AGN11CPY DS    XL1               COSTING COMPANY                                
AGN11UL  DS    CL2               UNIT/LEDGER '11'                               
AGN11REC DS    CL12              '11' RECORD                                    
AGNCONNM DS    CL20              CONTRA ACCOUNT NAME                            
AGNLNQ   EQU   *-AGND          LENGTH                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN 1C/11 TABLE 2                                    *         
***********************************************************************         
         SPACE 1                                                                
SJRECD   DSECT                                                                  
SJ1CKY   DS    0CL15           COSTING ACCOUNT KEY                              
SJ1CCPY  DS    XL1               COSTING COMPANY                                
SJ1CUL   DS    CL2               UNIT/LEDGER '1C'                               
SJ1CTYP  DS    CL1               TYPE                                           
SJ1COFF  DS    CL2               OFFICE                                         
SJ1CCLI  DS    CL3               CLIENT CODE                                    
SJ1CPRD  DS    CL6               PRODUCT CODE                                   
SJ11KY   DS    CL15            CONTRA  ACCOUNT KEY                              
SJMOS    DS    CL2             BILLING MONTH IN PACKED FORMAT                   
SJKLNQ   EQU   *-SJRECD        LENGTH OF KEY                                    
SJBKT    DS    PL8                 BUCKET                                       
SJBKLN   EQU   *-SJBKT             BUCKET LENGTH                                
SJBKCT   EQU   (*-SJBKT)/SJBKLN    NUMBER OF BUCKETS                            
SJMTH    DS    CL6             BILLING MONTH                                    
SJCSTNM  DS    CL20            COSTING ACCOUNT NAME                             
SJCONNM  DS    CL20            CONTRA ACCOUNT NAME                              
SJLNQ    EQU   *-SJRECD        LENGTH                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* PRINT DESCT                                                         *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                  PRINT LINE                                   
         DS    CL2                                                              
PCLT     DS    CL3                 CLIENT CODE                                  
         ORG   PCLT                                                             
         DS    CL2                                                              
PPRD     DS    CL3                 PRODUCT CODE                                 
         DS    CL3                                                              
PCPANAM  DS    CL41                CLIENT/PRODUCT/AGENCY NAME                   
PUNDLNQ  EQU   *-PRTLNE            UNDERLINE SEGMENT                            
         DS    CL5                                                              
PSYSMD   DS    CL2                 SYSTEM ID (N,P,S) + MEDIA                    
         DS    CL5                                                              
PMED     DS    CL12                MEDIA                                        
         DS    CL5                                                              
PMNTH    DS    CL6                 MEDIA                                        
         DS    CL5                                                              
PBAMNT   DS    CL17                BILLING AMOUNT                               
         DS    CL5                                                              
PRTERRS  DS    CL4                 ERRORS RETURNED                              
         DS    CL5                                                              
PRLNQ    EQU   *-PLINED            LENGTH                                       
         ORG   PRTLNE                                                           
         DS    CL2                                                              
P21CKY   DS    CL14                COSTING ACCOUNT KEY                          
         DS    CL6                                                              
P211KY   DS    CL14                CONTRA ACCOUNT KEY                           
         DS    CL6                                                              
P2MOS    DS    CL6                 BILLING MONTH                                
         DS    CL5                                                              
P2BLAMT  DS    CL17                BILLING AMOUNT                               
         DS    CL5                                                              
P21CTOT  DS    CL17                TOTAL FOR COSTING ACCOUNT                    
PRLNQ2   EQU   *-PLINED            LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* SPOT/NET FILE ++INCLUDES AND DSECTS                                 *         
***********************************************************************         
         SPACE 1                                                                
*        SPOT CLIENT DSECT                                                      
*                                                                               
SPCLID   DSECT                                                                  
*          DATA SET SPGENCLT   AT LEVEL 054 AS OF 12/09/94                      
CLTHDR   DS    0C                                                               
CKEY     DS    0CL13     V         KEY                                          
CKEYTYPE DS    CL1       B         RECORD TYPE X'00'                            
CKEYAM   DS    CL1       A/M       AGENCY/MEDIA                                 
CKEYCLT  DS    CL2       CLT       CLIENT CODE                                  
CKEYZRO  DS    CL9       B         BINARY ZEROS                                 
         SPACE 2                                                                
CLEN     DS    CL2       B         RECORD LENGTH (1000)                         
CCNTRL   DS    CL1       B         CONTROL BYTE                                 
CLINKS   DS    CL4       B         LINK FIELDS                                  
         DS    CL2       B         SPARE                                        
         DS    CL2       CLT       CLIENT INTERFACE                             
         SPACE 2                                                                
CNAME    DS    CL20      A         CLIENT NAME                                  
COFFICE  DS    CL1       N         OFFICE NUMBER                                
CPROF    DS    CL15      A/N       CLIENT PROFILE (SEE MANUAL)                  
CLIST    DS    880C      V         PRODUCT CODE LIST                            
*                        A         4 BYTE FIELDS  1-3=PRODUCT MNEMONIC          
*                        B                          4=PRODUCT NUMBER            
CCLTIFC  DS    CL8       A/N       NEW CLIENT INTERFACE CODE                    
CACCOFC  DS    CL2       A/N       2 CHAR ACC OFFICE CODE                       
*                                                                               
CGRP1    DS    CL3                 CLTGRP ASSGN                                 
CGRP2    DS    CL3                 CLTGRP ASSGN                                 
CGRP3    DS    CL3                 CLTGRP ASSGN                                 
CGRP4    DS    CL3                 CLTGRP ASSGN                                 
CGRP5    DS    CL3                 CLTGRP ASSGN                                 
*                                                                               
CLOCK    DS    CL1                 CLIENT LOCK                                  
CMCLTCOD DS    XL2                 MASTER TRAFFIC CLIENT CODE                   
CMCLTUNQ DS    XL1                 MASTER TRAFFIC CLIENT UNIQUE SEQNUM          
CMCLTPRD DS    XL1                 MASTER TRAFFIC CLIENT PRODUCT CODE           
*                                                                               
CACCAGY  DS    CL2                 ACC AGENCY OVERRIDE                          
CPOLONLY DS    CL1                 POL BUYING ONLY                              
*                                                                               
CCLTINTR DS    CL2       CLT       CLIENT INTERFACE                             
CEXTRA   DS    CL15      A/N       EXTRA PROFILE                                
CTITLE   DS    CL10                ID TITLE                                     
*                                                                               
CPU1     DS    CL20                PRODUCT USER FIELD DESC 1                    
CPU1TYPE DS    CL1                 PRODUCT USER TYPE (A/C/N)                    
CPU1LEN  DS    XL1                 PRODUCT USER LENGTH (MAX32)                  
CPU1FLG1 DS    XL1                                                              
CFLGREQQ EQU   X'80'               X'80' = REQUIRED                             
CFLGA2Q  EQU   X'40'               X'40' = SHOW ON A2                           
CFLGNIBQ EQU   X'20'               X'20' = (NET) INTEG BILLS                    
CFLGSPQ  EQU   X'10'               X'10' = (SPOT) SHOW ON BILLS                 
CFLGNTBQ EQU   X'10'               X'10' = (NET) TIME BILLS                     
CFLGMXQ  EQU   X'08'               X'08' = TRANSFER ON MX                       
CFLGNSBQ EQU   X'04'               X'04' = (NET) SPEC CHARGE BILLS              
*                                                                               
CPU1FLG2 DS    XL1                                                              
CUSERLNQ EQU   *-CPU1                                                           
*                                                                               
CPU2     DS    CL20                PRODUCT USER FIELD DESC 2                    
CPU2TYPE DS    CL1                 PRODUCT USER TYPE (A/C/N)                    
CPU2LEN  DS    XL1                 PRODUCT USER LENGTH (MAX16)                  
CPU2FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CPU2FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
*                                                                               
CEU1     DS    CL20                ESTIMATE USER FIELD DESC 1                   
CEU1TYPE DS    CL1                 ESTIMATE USER TYPE (A/C/N)                   
CEU1LEN  DS    XL1                 ESTIMATE USER LENGTH (MAX32)                 
CEU1FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CEU1FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
*                                                                               
CEU2     DS    CL20                ESTIMATE USER FIELD DESC 2                   
CEU2TYPE DS    CL1                 ESTIMATE USER TYPE (A/C/N)                   
CEU2LEN  DS    XL1                 ESTIMATE USER LENGTH (MAX16)                 
CEU2FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CEU2FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
CULNQ    EQU   *-CPU1                                                           
*                                                                               
CMEDNAME DS    CL10                MEDIA NAME OVERRIDE                          
CNETID   DS    CL4                 NETWORK ID                                   
COPT1    DS    XL1                                                              
COP1COSQ EQU   X'80'               SECOND COST REQUIRED                         
COP1INFQ EQU   X'40'               INFOMERCIAL                                  
COP1DBLQ EQU   X'20'               DO NOT TEST DOUBLE-BOOKING                   
COP1MGRQ EQU   X'10'               REQUIRE MGREQ REC IF ID=MKTGRP               
COP1NMG  EQU   X'08'               CLIENT USES NEW MAKEGOODS                    
COP1CTAQ EQU   X'04'               CONTRACT ANALYSIS (CTA) CLIENT               
COPT2    DS    XL1                                                              
         DS    CL2                 SPARE                                        
*                                                                               
CPST     DS    CL10                PST CODES                                    
CDAILY   DS    CL1                 ESTIMATES WILL BE DAILY                      
CPWPCT   DS    XL3                 PROFIT WITHIN PERCENTAGE                     
CZENCLT  DS    CL3                 ZENITH CLIENT CODE                           
         DS    149C                ** NEW SPARE **                              
CLTHDRL  EQU   *-CLTHDR                                                         
         SPACE 1                                                                
* CONTENTS OF CPROF:                CONTENTS OF CEXTRA:                         
*   1 - BRAND/POL TRNDS               1 - CANADIAN DEMO OPTION                  
*   2 - LOCK BOX NUMBER               2 - CANADIAN NETWORK TAX                  
*   3 - MKT/STA TRNDS                 3 - BUY ID REQUIRED                       
*   4 - RATING SERVICE                4 - ESTIMATE FILTERS REQ                  
*   5 - BILL FORMULA CNTRL            5 - CAMPAIGNS                             
*   6 - BILL ESTIMATE CNTRL           6 - U.S. SPILL                            
*   7 - PRINT CLT CODE AS AAN         7 - 'EST=NO' EST NAME                     
*   8 - PRINT EST SERIES NM           8 - MKGDS IN MISSED MTH                   
*   9 - GOALS CPP OVERRIDE            9 - GOAL REQD FOR BUY                     
*   10- PROGRAM ADJ. CNTRL            10- COUNTRY                               
*   11- POL TIMESHEET DEMOS           11- OUT-OF-WEEK CLIENT                    
*   12- FORCE EST SERIES REQ          12- GST CODE                              
*   13- PRD REQ FOR TRUE POL          13- SPECIAL DEMO ADJ.                     
*   14- EXCLUSION GROUP CODE          14- PRD REQD FOR ADDS SEND                
*   15- CLIENT RATE CNTRL             15- RATE COVERAGE CONTROL (NET)           
*                                                                               
*        SPOT PRODUCT DSECT                                                     
*                                                                               
SPPRDD   DSECT                                                                  
*          DATA SET SPGENPRD   AT LEVEL 014 AS OF 05/11/93                      
*              PRODUCT HEADER RECORD                                            
         SPACE 1                                                                
PRDHDR   DS    0C                                                               
PKEY     DS    0CL13     V         KEY                                          
PKEYTYPE DS    CL1       B         RECORD TYPE X'00'                            
PKEYAM   DS    CL1       A/M       AGENCY/MEDIA                                 
PKEYCLT  DS    CL2       CLT       CLIENT CODE                                  
PKEYPRD  DS    CL3       A         PRODUCT CODE                                 
PKEYZRO  DS    CL6       B         BINARY ZEROS                                 
         SPACE 2                                                                
PLEN     DS    CL2       B         RECORD LENGTH (240)                          
PCNTRL   DS    CL1       B         CONTROL BYTE                                 
PLINKS   DS    CL4       B         LINK FIELDS                                  
         DS    CL4       B         SPARE                                        
         SPACE 2                                                                
PACCT    DS    CL4       A/N       ACCOUNT NUMBER                               
PNAME    DS    CL20      A         PRODUCT NAME                                 
PCODE    DS    CL2       B         PRODUCT CODE                                 
PADDR1   DS    CL30      A/N       BILL ADDRESS LINE 1                          
PADDR2   DS    CL30      A/N       BILL ADDRESS LINE 2                          
PADDR3   DS    CL30      A/N       BILL ADDRESS LINE 3                          
PADDR4   DS    CL30      A/N       BILL ADDRESS LINE 4                          
PDIV     DS    CL3       A/N       DIVISION CODE                                
PBILLDT  DS    CL2       B         EFFECTIVE Y/M OF SERVICE                     
PBILLBAS DS    CL1       B         2 4-BIT FIELDS - BILL BASE/COMM BASE         
*                                  B'0000' = GROSS, B'0001' = NET               
PBILLCOM DS    CL4       B         SIGNED COMMISSION (99.9999)                  
PAGYFEE  DS    CL2       P         OTHER AGENCY FEE (2 IMPLIED DEC)             
PPROF    DS    CL30      A/N       PROFILE                                      
PGRP1    DS    CL3                 PRDGRP ASSGN                                 
PGRP2    DS    CL3                 PRDGRP ASSGN                                 
PGRP3    DS    CL3                 PRDGRP ASSGN                                 
PCLASS   DS    CL1                 PRODUCT CLASS                                
PGRP4    DS    CL3                 PRDGRP ASSGN                                 
PGRP5    DS    CL3                 PRDGRP ASSGN                                 
PLOCK    DS    CL1                 PRD LOCKED                                   
PLKDAT   DS    CL2                 PRD LOCK ACTV DATE  (COMPRESSD)              
PGSTCODE DS    CL1                 GOODS AND SERVICE TAX                        
         DS    CL8                 SPARE                                        
PUSER1   DS    XL32                USER FIELD 1                                 
PUSER2   DS    XL16                USER FIELD 2                                 
PPST     DS    CL10                PST CODES                                    
PTALAGY  DS    CL6                 TALENT AGENCY                                
         DS    CL32                SPARE                                        
PRDHDRL  EQU   *-PRDHDR                                                         
*                                                                               
*        SPOT BILLING RECORD DSECT                                              
*                                                                               
SPBILD   DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
***********************************************************************         
* PRINT FILE ++INCLUDES AND DSECTS                                    *         
***********************************************************************         
         SPACE 1                                                                
*        PRINT CLIENT DSECT                                                     
*                                                                               
PRCLID   DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
*                                                                               
*        PRINT PRODUCT DSECT                                                    
*                                                                               
PRPRDD   DSECT                                                                  
*          DATA SET PPRDREC    AT LEVEL 024 AS OF 06/01/93                      
PPRDREC  DS    0C .                *** - PRODUCT RECORD ***                     
***                                                                             
***      WARNING DO NOT LET RECORD LENGTH EXCEED 330 BYTES                      
***      PPNEWFILE CURRENTLY ALLOWS FOR 330 BYTES                               
***      CURRENT MAXIMUM RECORD LENGTH COULD BE 300 BYTES                       
***      WHEN ALL OPTIONAL ELEMENTS ARE PRESENT                                 
***                                                                             
PPRDKEY  DS    0CL25                                                            
PPRDKAGY DS    CL2 .     A         AGENCY CODE                                  
PPRDKMED DS    CL1 .     A         PRINT MEDIA CODE                             
PPRDKRCD DS    X'06' .   B         RECORD CODE                                  
PPRDKCLT DS    CL3 .     A         CLIENT CODE                                  
PPRDKPRD DS    CL3 .     A         PRODUCT CODE                                 
         DS    15X'00'                                                          
*                                                                               
PPRDLEN  DS    CL2 .     B         RECORD LENGTH                                
*                                                                               
PPRDCNTL DS    CL2 .     B         CONTROL BYTES                                
         DS    CL4 .     B         DISK ADDRESS FOR LINKED RECORDS              
*                                                                               
PPRDELEM DS    0CL200                                                           
         DS    X'06' .   B         ELEMENT CODE                                 
         DS    X'C8' .   B         ELEMENT LENGTH                               
PPRDNAME DS    CL20 .    AN        PRODUCT NAME                                 
PPRDBILL DS    CL20 .    AN        BILL RECEIPT NAME                            
PPRDLIN1 DS    CL30 .    AN        ADDRESS - LINE 1                             
PPRDLIN2 DS    CL30 .    AN        ADDRESS - LINE 2                             
PPRDATTN DS    CL24 .    AN        ATTENTION                                    
PPRDDIV  DS    CL3 .     N         DIVISION CODE                                
PPRDACCT DS    CL4 .     AN        ACCOUNT NUMBER                               
*                        IF FIRST POS = X'FF' THEN NEXT 3 ARE BINARY            
*                        FOR JWT PRODUCT INTERFACE NUMBER                       
PPRDBILP DS    CL37 .    AN        BILLING PROFILE                              
PPRDEXCL DS    CL3 .     A         EXCLUSION CODE                               
PPRDOAN  DS    CL2 .     A         OAN (OTHER AGENCY NAME) AGENCY CODE          
PPRDBIL2 DS    CL20 .    A/N       BILL RECEIPT NAME  - LINE 2                  
PPRDGST  DS    CL1       A         CANADIAN GST TAX CODE                        
*                                  X'00' OR C'S',C'X',C'Z'                      
         DS    CL04 .              SPARE                                        
*                                                                               
PPRDUDEF DS    0CL50                                                            
         DS    X'08'     B         ELEMENT CODE                                 
         DS    X'32'     B         ELEMENT LENGTH                               
PPUSER1  DS    XL32      A         USER DESCRIPTION 1                           
PPUSER2  DS    XL16      A         USER DESCRIPTION 2                           
*                                                                               
PPRDPST  DS    0CL12                                                            
         DS    X'25'     B         ELEMENT CODE                                 
         DS    X'0C'     B         ELEMENT LENGTH                               
PPRDPSTC DS    CL10      A         PST CODES                                    
*                                                                               
*        PRINT BILLING RECORD DSECT                                             
*                                                                               
PRBILD   DSECT                                                                  
       ++INCLUDE PBILLREC                                                       
         EJECT                                                                  
***********************************************************************         
* ADDTRANS DSECT AND ++INCLUDE                                        *         
***********************************************************************         
         SPACE 1                                                                
TRNBLKD  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPMB02 10/03/03'                                      
         END                                                                    
