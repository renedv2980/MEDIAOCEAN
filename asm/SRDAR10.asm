*          DATA SET SRDAR10    AT LEVEL 009 AS OF 06/24/19                      
*PHASE T16110B                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE REREPDIF                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE REDARTKO                                                               
*INCLUDE REGENDHT                                                               
         PRINT NOGEN                                                            
         TITLE '$RDR (T16110) -- REP DARE INTERFACE'                            
**********************************************************************          
* THIS IS AN ONLINE PROGRAM TO RETRIEVE AND FORMAT ONTO REP FILE     *          
*        DARE AGENCY ORDERS.                                         *          
**********************************************************************          
*                                                                    *          
* OCT10/12 (HWON)--- COPIED FROM SRRDR00 AS THAT PROGRAM IS NOW      *          
*                    DEFUNCT.  INSTEAD OF BEING CALLED BY SRTIM00 OR *          
*                    TIMER-POP, THIS PROGRAM WILL NOW BE CALLED BY   *          
*                    SRDAR00.                                        *          
*                                                                    *          
*                    ***  END TOMBSTONE  ***                         *          
**********************************************************************          
SRRDR00  CSECT                                                                  
         NMODL WORKX-WORKD,$D10*,R8,RR=R3                                       
         USING WORKD,RC                                                         
         ST    R3,RELO                                                          
         ST    RD,SAVERD                                                        
         ST    R1,SAVER1                                                        
         MVI   RETCODE,0           RESET ERROR RETURN BYTE                      
*                                                                               
* INITIALIZATION                                                                
*                                                                               
         L     RE,0(R1)                                                         
         MVC   SRPARS,0(RE)        SAVE S/R PARAMETER LIST                      
*                                                                               
SRPARMSD USING SRPARMD,SRPARS                                                   
*                                                                               
         L     R9,SRPAR1                                                        
         USING SYSFACD,R9          A(SYSFACS)                                   
*                                                                               
         MVC   ATIA,SRPAR6         A(TIA)                                       
         MVC   AUTL,SRPAR3         A(UTL)                                       
         L     RA,ATIA                                                          
         USING T162FFD,RA          A(TWA)                                       
         L     R6,SRPAR4                                                        
         ST    R6,VCOMFACS                                                      
         USING COMFACSD,R6         A(COMFACS)                                   
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VCALLOVL,CCALLOV                                                 
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VADDAY,CADDAY                                                    
         DROP  R6                                                               
         XC    POWERCDE,POWERCDE                                                
         XC    AUDITYP,AUDITYP                                                  
*                                                                               
         LR    RF,RC               ESTABLISH A(SPOOLAREA)                       
         AH    RF,=Y(SPULAREA-WORKD)                                            
         ST    RF,ASPLAREA                                                      
*                                                                               
         LR    RF,RC               ESTABLISH A(IOAREA2)                         
         AH    RF,=Y(IOAREA2-WORKD)                                             
         ST    RF,AIO2                                                          
*                                                                               
         LR    RF,RC               ESTABLISH A(PQREC)                           
         AH    RF,=Y(PQREC-WORKD)                                               
         ST    RF,APQREC                                                        
*                                                                               
         LR    RF,RC               ESTABLISH A(CONAREA)                         
         AH    RF,=Y(CONAREA-WORKD)                                             
         ST    RF,ACONAREA                                                      
*                                                                               
         LR    RF,RC               ESTABLISH A(MQAREA)                          
         AH    RF,=Y(MQAREA-WORKD)                                              
         ST    RF,AMQAREA                                                       
*                                                                               
         LR    RF,RC               ESTABLISH A(PASSIVE POINTER)                 
         AH    RF,=Y(PASAREA-WORKD)                                             
         ST    RF,APASAREA                                                      
*                                                                               
         XC    DMCB(12),DMCB       SET A(SPOOL)                                 
         MVC   DMCB+4,=X'D9000A0C' SPOOL                                        
         GOTO1 VCALLOVL,DMCB                                                    
         MVC   ASPOOL,DMCB         LOAD A(SPOOL)                                
*                                                                               
         MVC   DATADISP,=H'34'                                                  
         GOTO1 VCALLOVL,DMCB,0,X'D9000AAC',0                                    
         MVC   VREPFACS,0(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
*                                                                               
* FOLLOWING AREA MUST BE CLEARED BETWEEN PROCESSING INCOMING AGY ORDERS         
*                                                                               
         XC    AUDITYP,AUDITYP                                                  
         XC    SPCODE,SPCODE                                                    
         XC    SVSPCODE,SVSPCODE                                                
         XC    OLDSTA,OLDSTA                                                    
         L     RE,APASAREA                                                      
         LA    RF,L'PASAREA                                                     
         XCEFL                                                                  
******                                                                          
* R7 IS STILL POINTING TO THE EDICT ENTRY FROM SRDAR00                          
******                                                                          
         USING PAGYHDRD,R7                                                      
         XC    WORK,WORK                                                        
         CLC   =C'AGYHDR',PAHDTID                                               
         BNE   SRRD0010                                                         
***                                                                             
* EDICT ENTRY IS AGYHDR                                                         
***                                                                             
         GOTO1 VHEXIN,DMCB,PAHDPQUR,WORK,4    HEX ID # OF USER                  
         OC    WORK(2),WORK                                                     
         BZ    EXIT                                                             
         MVC   WORK+2(3),PAHDPQSB                                               
         GOTO1 VHEXIN,DMCB,PAHDPQRE,WORK+5,4                                    
         B     SRRD0020                                                         
*                                                                               
SRRD0010 CLC   =C'AGYCAN',PAHDTID                                               
         BNE   SRRD0015                                                         
***                                                                             
* EDICT ENTRY IS AGYCAN                                                         
***                                                                             
         USING PAGYCAND,R7                                                      
         GOTO1 VHEXIN,DMCB,PACNPQUR,WORK,4    HEX ID # OF USER                  
         OC    WORK(2),WORK                                                     
         BZ    EXIT                                                             
         MVC   WORK+2(3),PACNPQSB                                               
         GOTO1 VHEXIN,DMCB,PACNPQRE,WORK+5,4                                    
         B     SRRD0020                                                         
***                                                                             
* EDICT ENTRY IS MKGAPP OR MKGREJ                                               
***                                                                             
         USING MOFRAPPD,R7                                                      
SRRD0015 GOTO1 VHEXIN,DMCB,MOAPPQUR,WORK,4    HEX ID # OF USER                  
         OC    WORK(2),WORK                                                     
         BZ    EXIT                                                             
         MVC   WORK+2(3),MOAPPQSB                                               
         GOTO1 VHEXIN,DMCB,MOAPPQRE,WORK+5,4                                    
         DROP  R7                                                               
*                                                                               
SRRD0020 LA    R2,WORK                                                          
         GOTO1 =A(CTRLSET),DMCB,(R2),RR=RELO                                    
         BNE   SRRD0900                                                         
                                                                                
SRRD0030 GOTOR RETRVREP,DMCB,(R2)  GET THE REPORT FROM PRINT QUEUE              
         BNE   SRRD0050            RECORD NOT FOUND                             
         BAS   RE,UPDATDAR         MARK REPORT AS PRINTED                       
*                                                                               
SRRD0050 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(0,=C'COMMIT'),0  COMMIT                           
SRRD0900 EQU   *                                                                
         CLC   =H'309',ERRCODE     IF SYSTEM IS NOT UP,                         
         BNE   SRRD0910                                                         
         BRAS  RE,RDPQREP          READ PRINT QUEUE REPORT TO GET DATA          
         BNE   SRRD0910            IF CC NEQ DON'T SEND MESSAGE                 
         GOTO1 =A(SNDERROR),DMCB,309,RR=RELO  REP SYSTEM NOT UP                 
*                                                                               
SRRD0910 L     R1,SAVER1                                                        
         MVC   4(1,R1),RETCODE     SET RETCODE IN CALLER'S P2(1)                
         B     EXIT                                                             
*                                                                               
*RETCODE VALUES:                                                                
*01-CAN'T OPEN CTRL SYS, OR TARGET SE SYSTEM, NOT OPENED FOR WRITE              
*02-SE SYSTEM IS NOT NATIVE IN THIS FACPAK                                      
         EJECT                                                                  
*                                                                               
*  UPDATDAR:  RETRIEVES LINES OF REPORT, FORMATS AND WRITES NEW                 
*    DARE RECORDS TO THE REP FILE..                                             
*                                                                               
UPDATDAR NTR1                                                                   
         XC    ERRCODE,ERRCODE                                                  
         LA    R4,PQINDEX          SET INDEX FOR REPORT                         
         USING UKRECD,R4                                                        
*                                                                               
         XC    LASTREC,LASTREC     SET OUTPUT FLAGS                             
         XC    INPROG,INPROG                                                    
         MVI   MGINPROG,C'N'       TURN OFF 'MAKEGOOD REJECT' FLAG              
         MVI   EMAILFLG,EMFOK                                                   
*                                                                               
UPDA0040 EQU   *                                                                
         XC    R,R                                                              
         GOTO1 VDATAMGR,DMCB,(0,=C'READ'),UKUSRINF,0,R,APQREC,0                 
         CLI   DMCB+8,0            END OF PQ CONTROL INTERVAL?                  
         BNE   UPDA0900            YES - FINISHED - CHECK OUTPUT                
******   ZIC   RF,TESTCTR          **TEST**                                     
******   LA    RF,1(RF)            **TEST**                                     
******   STC   RF,TESTCTR          **TEST**                                     
******   CLI   DUMPCTR,0           DUMP COUNTER = ZERO?                         
******   BE    UPDA0060            YES - DON'T DUMP                             
******   CLC   TESTCTR,DUMPCTR     DUMP = TEST?                                 
******   BNE   *+6                 NO  - DON'T DUMP                             
******   DC    H'0'                                                             
UPDA0060 EQU   *                                                                
         LA    R3,RECTABLE         A(CONVERSION TABLE)                          
UPDA0080 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE REACHED?                        
         BE    UPDA0040            YES - SKIP RECORD - READ NEXT                
         CLC   0(6,R3),R+1         NO  - LOOK FOR RECORD TYPE                   
         BE    UPDA0120            FOUND - PROCESS IT                           
         LA    R3,LRECTAB(R3)      BUMP TO NEXT ENTRY                           
         B     UPDA0080            CHECK NEXT TYPE IN TABLE                     
UPDA0120 EQU   *                                                                
         L     RF,8(R3)            LOAD ROUTINE ADDRESS                         
         A     RF,RELO                                                          
         BASR  RE,RF               BRANCH TO ROUTINE                            
*                                                                               
         CLC   =H'308',ERRCODE     CANNOT HANDLE BUY NUMBER                     
         BE    UPDA0123            GET NEXT RECORD                              
*                                                                               
         OC    ERRCODE,ERRCODE     ERROR ENCOUNTERED, SEND ERRNOT               
         BNZ   UPDA0950                                                         
*                                                                               
UPDA0123 TM    MQFLAGS,X'80'       ARE WE MQ'ING EDI TO MO?                     
         BZ    UPDA0040                                                         
*                                                                               
         L     RF,MQOFFSET         CHECK IF EDI TOO BIG                         
         ZIC   RE,12(R3)                                                        
         AR    RF,RE                                                            
         C     RF,=F'3200'                                                      
         BNH   UPDA0125                                                         
         NI    MQFLAGS,X'FF'-X'80' TOO BIG, DON'T SEND THIS EDI                 
         B     UPDA0040                                                         
*                                                                               
UPDA0125 EQU   *                                                                
         L     RF,AMQAREA          BUFFER FOR MQ STUFF                          
         L     RE,MQOFFSET                                                      
         AR    RF,RE                                                            
         ZIC   RE,12(R3)                                                        
         LTR   RE,RE                                                            
         BZ    UPDA0040                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),R+1                                                      
*                                                                               
* REMOVE TRAILING SPACES                                                        
*                                                                               
         AR    RF,RE                                                            
         ZIC   R1,12(R3)                                                        
UPDA0130 CLI   0(RF),C' '                                                       
         BNE   UPDA0140                                                         
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         BCT   R1,UPDA0130                                                      
*                                                                               
UPDA0140 DS    0H                                                               
         MVC   1(2,RF),=X'0D25'    CRLF                                         
         L     RF,MQOFFSET                                                      
         LA    RF,3(RE,RF)         1 FOR EX AND 2 FOR CRLF                      
         ST    RF,MQOFFSET                                                      
*                                                                               
         CLC   =C'AGYTLR',R+1      LAST RECORD, PUT MQ BUFFER                   
         BNE   UPDA0040                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CMQIO-COMFACSD(RF)                                            
         L     R2,MQOFFSET                                                      
         GOTOR (RF),DMCB,=CL8'PUT',AMQAREA,(R2),0,0,DUB                         
*                                                                               
         B     UPDA0040            GO BACK FOR NEXT RECORD                      
UPDA0900 EQU   *                                                                
         CLI   MGINPROG,C'Y'       MG REJECT IN PROGRESS?                       
         BNE   UPDA0950            NO                                           
         BAS   RE,RECWRITE         YES - WRITE THE FINAL RECORD                 
         GOTO1 =A(UPDTRIS),RR=RELO UPDATE RIS PASSIVE KEYS                      
*                                                                               
*   MG REJECTS MAY HAVE COMMENTS.  IF A MG REJECT IS IN PROGRESS,               
*        IT WILL BE WRITTEN TO THE FILE AT THE END OF THE REPORT.               
*                                                                               
UPDA0950 EQU   *                                                                
*&&DO                                                                           
         TM    DARFLAGS,DFXML                                                   
         BZ    UPDA0970                                                         
         CLI   EMAILFLG,EMFOK                                                   
         BE    UPDA0970                                                         
*                                                                               
         XC    ERREMAIL,ERREMAIL                                                
UPDA0960 EQU   *                                                                
         XC    R,R                                                              
         GOTO1 VDATAMGR,DMCB,(0,=C'READ'),UKUSRINF,0,R,APQREC,0                 
         CLI   DMCB+8,0            END OF PQ CONTROL INTERVAL?                  
         BNE   UPDA0970            YES - FINISHED - CHECK OUTPUT                
         CLC   =C'AGYCOM',R+1                                                   
         BNE   UPDA0960                                                         
         CLC   =C'+E',R+15         LOOKING FOR ERROR EMAIL ADDRESS              
         BNE   UPDA0960                                                         
         MVC   ERREMAIL,R+17                                                    
*&&                                                                             
*                                                                               
UPDA0970 EQU   *                                                                
*        GOTO1 VDATAMGR,DMCB,(0,=C'PRINTED'),UKUSRINF,PQINDEX,R,APQREC          
         GOTO1 VDATAMGR,DMCB,(0,=C'PRIRET'),UKUSRINF,PQINDEX,R,APQREC           
         CLI   DMCB+8,0            MARKED AS 'PRINTED'?                         
         BE    *+6                 YES                                          
         DC    H'0'                NO                                           
*                                  SEND ERRNOT, MARK REPORT AND EXIT            
         OC    ERRCODE,ERRCODE     ERROR ENCOUNTERED, SEND ERRNOT               
         BZ    EXIT                                                             
         CLC   =C'SA',ERRCODE      SELF-APPLY OFFER FLAG?                       
         BE    UPDA1000                                                         
*                                                                               
         CLC   =C'XX',ERRCODE      XML OK TO AGENCY?                            
         BE    UPDA0980                                                         
*                                                                               
         TM    EMAILFLG,EMFDUP+EMFERBUY                                         
         BNZ   *+8                                                              
         MVI   EMAILFLG,EMFERROR                                                
*                                                                               
*        TM    DARFLAGS,DFXML                                                   
*        BNZ   UPDA0990                                                         
*                                                                               
UPDA0980 EQU   *                                                                
         GOTO1 =A(SNDERROR),DMCB,0,RR=RELO                                      
*                                                                               
         MVI   EMAILFLG,EMFOK                                                   
*                                                                               
         TM    DARFLAGS,DFXML                                                   
         BZ    EXIT                                                             
*                                                                               
* FOR XML ORDER, SEND ERROR/OK BACK TO AGENCY                                   
*                                                                               
UPDA0990 EQU   *                                                                
*        GOTOR XMLEMAIL                                                         
         B     EXIT                                                             
*                                                                               
UPDA1000 EQU   *                                                                
         GOTOR DOMKGROK                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*   AGYHDR:                                                                     
***********************************************************************         
AGYHDR   NTR1                                                                   
         GOTOR AGYHDR00                                                         
         B     EXIT                                                             
*                                                                               
*   AGYDS1:   AGENCY DESCRIPTION 1 RECORD                                       
*                                                                               
AGYDS1   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYDS1D,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   RDARAGNM,PAD1AGNM   INSERT AGENCY NAME                           
         MVC   RDARAGAD,PAD1AGAD   INSERT AGENCY ADDRESS                        
*                                                                               
*        ALL FIELDS FROM AGYDS1 RECORD LOADED TO NEW FORMAT.                    
*        NOTHING TO WRITE OUT AT THIS TIME.                                     
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*   AGYDS2:   AGENCY DESCRIPTION 2 RECORD                                       
*                                                                               
AGYDS2   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYDS2D,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   RDARCLNM,PAD2CLTN   INSERT CLIENT NAME                           
         MVC   RDARESDS,PAD2ESTN   INSERT ESTIMATE DESCRIPTION                  
         DROP  R2                                                               
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
DEMO     USING RDARDMEL,ELTAREA                                                 
         MVI   DEMO.RDARDMCD,X'03'                                              
         MVI   DEMO.RDARDMLN,RDARDMLQ                                           
*                                                                               
         MVC   DEMO.RDARDEM1(16),PAD2TDEM                                       
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA                    
*                                                                               
*        ALL FIELDS FROM AGYDS2 RECORD LOADED TO NEW FORMAT.                    
*        NOTHING TO WRITE OUT AT THIS TIME.                                     
*                                                                               
         B     EXIT                                                             
         DROP  DEMO,R4                                                          
         EJECT                                                                  
*                                                                               
*   AGYDS3:   AGENCY DESCRIPTION 3 RECORD                                       
*                                                                               
AGYDS3   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYDS3D,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   RDARPRN1,PAD3PRDN   INSERT PRODUCT NAME                          
         MVC   RDARPRN2,PAD3PR2N   INSERT PIGGY PRODUCT NAME                    
*                                                                               
         CLC   =C'***',RDARPRD1    IF POOL/VAR, FORCE *CORPORATE*/              
         BNE   AGYDS3X             *VARIOUS* IN PRODUCT NAME FIELD              
         XC    RDARPRN1,RDARPRN1                                                
         MVC   RDARPRN1,=CL34'*CORPORATE*'                                      
         TM    RDARMISC,X'10'                                                   
         BZ    AGYDS3X                                                          
         MVC   RDARPRN1,=CL34'*VARIOUS*'                                        
*                                                                               
*        ALL FIELDS FROM AGYDS3 RECORD LOADED TO NEW FORMAT.                    
*        NOTHING TO WRITE OUT AT THIS TIME.                                     
*                                                                               
AGYDS3X  DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R4                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   AGYDS4:   AGENCY DESCRIPTION 4 RECORD                                       
*                                                                               
AGYDS4   NTR1                                                                   
         GOTOR AGYDS400                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   AGYDS5:   AGENCY DESCRIPTION 5 RECORD                                       
*                                                                               
AGYDS5   NTR1                                                                   
         GOTOR AGYDS500                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   AGYEDI:   EXTRA EDI ELEMENTS FOR PASSTHROUGH OF ADDITIONAL DATA             
*                                                                               
AGYEDI   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYDS1D,R4         USE DSECT FROM AGYDS1                        
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         XC    ELTAREA(100),ELTAREA                                             
*                                  CLEAR WORKSPACE                              
         LA    R5,ELTAREA                                                       
         USING RDAREDEM,R5                                                      
         MVI   RDAREDCD,X'ED'      INSERT ELEMENT CODE                          
         MVI   RDAREDLN,84         INSERT ELEMENT LENGTH                        
         MVC   RDAREDDT,PAD1AGNM   INSERT 80 CHARS OF DATA                      
         BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
*                                                                               
*        ALL FIELDS FROM AGYEDI RECORD LOADED TO NEW FORMAT.                    
*            NEXT RECORD TYPE WILL TRIGGER RECORD OUTPUT                        
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*   AGYSTD: AGENCY STANDARD COMMENT                                             
*                                                                               
AGYSTD   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYSTDD,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         CLC   INPROG,LASTREC      LAST RECORD WRITTEN OUT?                     
         BE    AGYS0040            YES                                          
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         MVI   RDARKRT,X'20'       SET REC TYPE TO 'STANDARD COMMENT'           
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR LOWER KEY                              
         XCEFL IOAREA+27,3973                                                   
*                                  CLEAR IOAREA: LEAVE KEY SET UP               
         LA    RF,37               SET INITIAL RECORD LENGTH                    
         STCM  RF,3,RDARLEN        INSERT INTO RECORD                           
         MVC   RDARELEM(2),=X'0103' SET ELEMENT CODE/LENGTH                     
AGYS0040 EQU   *                                                                
         XC    ELTAREA(100),ELTAREA                                             
*                                  CLEAR WORKSPACE                              
         LA    R5,ELTAREA                                                       
         USING RDARSCEL,R5                                                      
         MVI   RDARSCCD,2          INSERT ELEMENT CODE                          
         LA    R6,PASTTEXT+77      SCAN COMMENT FOR BLANKS                      
         LA    RF,78               LOOP CONTROL                                 
AGYS0080 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   AGYS0120            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,AGYS0080         LOOP THROUGH ALL                             
         MVI   RDARSCLN,4          BLANK LINE:  SET CONTROL TO 4                
         MVC   RDARSCCM(2),SPACESX INSERT TWO BLANKS                            
         B     AGYS0160                                                         
AGYS0120 EQU   *                                                                
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,AGYS0950         MOVE COMMENT BY LENGTH                       
         LA    RF,3(RF)            SET PROPER ELEMENT LENGTH                    
         STC   RF,RDARSCLN         INSERT LENGTH INTO ELEMENT                   
AGYS0160 EQU   *                                                                
         BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
         B     EXIT                                                             
*                                                                               
AGYS0950 MVC   RDARSCCM(0),PASTTEXT LOAD COMMENT BY LENGTH                      
*                                                                               
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*   AGYCOM:   ORDER COMMENT                                                     
*                                                                               
AGYCOM   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYCOMD,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         CLC   INPROG,LASTREC      LAST RECORD WRITTEN OUT?                     
         BNE   AGYC0020            YES                                          
         CLI   PACMCONT,C'+'       XML ORDER, CHECK FOR EMAIL ADDRESS           
         BE    AGYC0025                                                         
         B     AGYC0040                                                         
*                                                                               
AGYC0020 EQU   *                                                                
         CLI   PACMCONT,C'+'       XML ORDER, CHECK FOR EMAIL ADDRESS           
         BNE   AGYC0030                                                         
*                                                                               
* FOR XML ORDERS, AGYCOM IS USED TO PASS EMAIL ADDRESSES IF THE                 
* CONTINTUATION FIELD IS A '+'                                                  
*                                                                               
AGYC0025 EQU   *                                                                
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         GOTOR SETEMAIL                                                         
         B     EXIT                                                             
*                                                                               
AGYC0030 EQU   *                                                                
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         MVI   RDARKRT,X'30'       SET REC TYPE TO 'ORDER COMMENT'              
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR LOWER KEY                              
         XCEFL IOAREA+27,3973                                                   
*                                  CLEAR IOAREA: LEAVE KEY SET UP               
         LA    RF,37               SET INITIAL RECORD LENGTH                    
         STCM  RF,3,RDARLEN        INSERT INTO RECORD                           
         MVC   RDARELEM(2),=X'0103' SET ELEMENT CODE/LENGTH                     
*                                                                               
AGYC0040 EQU   *                                                                
         XC    ELTAREA(100),ELTAREA                                             
*                                  CLEAR WORKSPACE                              
         LA    R5,ELTAREA                                                       
         USING RDAROREL,R5                                                      
         MVI   RDARORCD,2          INSERT ELEMENT CODE                          
         LA    R6,PACMTEXT+77      SCAN COMMENT FOR BLANKS                      
         LA    RF,78               LOOP CONTROL                                 
AGYC0080 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   AGYC0120            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,AGYC0080         LOOP THROUGH ALL                             
         MVI   RDARORLN,4          BLANK LINE:  SET CONTROL TO 4                
         MVC   RDARORCM(2),SPACESX INSERT TWO BLANKS                            
         B     AGYC0160                                                         
AGYC0120 EQU   *                                                                
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,AGYC0950         MOVE COMMENT BY LENGTH                       
         LA    RF,3(RF)            SET PROPER ELEMENT LENGTH                    
         STC   RF,RDARORLN         INSERT LENGTH INTO ELEMENT                   
AGYC0160 EQU   *                                                                
         BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
         B     EXIT                                                             
*                                                                               
AGYC0950 MVC   RDARORCM(0),PACMTEXT                                             
*                                  LOAD COMMENT BY LENGTH                       
*                                                                               
         DROP  R2,R4,R5                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   BUYHDR:   BUY HEADER                                                        
*                                                                               
BUYHDR   NTR1                                                                   
*                                                                               
         ZICM  RF,BUYCOUNT,2       INCREMENT BUY COUNTER                        
         AHI   RF,1                                                             
         STCM  RF,3,BUYCOUNT                                                    
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PBUYHDRD,R4                                                      
*                                                                               
         LA    R2,IOAREA SET OUTPUT AREA DSECT                                  
         USING RDARREC,R2                                                       
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         CLC   INPROG,LASTREC      LAST RECORD WRITTEN OUT?                     
         BE    BUYH0040            YES                                          
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         MVI   RDARKRT,X'40'       SET REC TYPE TO 'BUY'                        
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR LOWER KEY                              
BUYH0040 EQU   *                                                                
         TM    DARFLAGS,DFXML                                                   
         BO    BUYH0050                                                         
*                                                                               
         PACK  DUB(8),PBHDBLIN(4)                                               
*                                  CONVERT AND STORE BUY LINE NUMBER            
*                                  ONE CHARACTER SHOULD BE SUFFICIENT           
         CVB   RF,DUB                                                           
         C     RF,=F'256'                                                       
         BL    BUYH0045                                                         
         MVC   ERRCODE,=H'308'     CANNOT HANDLE BUY NUMBER                     
         MVI   BUYHIFLG,C'Y'       SET FLAG                                     
BUYH0045 STC   RF,RDARKSEQ                                                      
         CLI   RDARKSEQ,0          SHOULD NEVER BE ZERO                         
         BNE   BUYH0060                                                         
         MVI   RDARKSEQ,X'FF'                                                   
         B     BUYH0060                                                         
*                                                                               
* FOR XML ORDERS, RENUMBER AGENCY BUY ALWAYS FROM 1-254                         
* AGENCY CAN SEND BUYS NUMBERING FROM 1-999                                     
*                                                                               
BUYH0050 EQU   *                                                                
         PACK  DUB(8),PBHDBLIN(4)                                               
         CVB   RF,DUB                                                           
         STCM  RF,3,ABUYNUM        SAVE OFF ACTUAL BUY NUMBER FOR XML           
*                                                                               
         MVC   RDARKSEQ,ABUYNDX                                                 
         ZIC   RF,ABUYNDX                                                       
         AHI   RF,1                                                             
         STC   RF,ABUYNDX                                                       
         CLI   ABUYNDX,255                                                      
         BNE   BUYH0060                                                         
         MVI   EMAILFLG,EMFERBUY   MAX BUY EXCEEDED                             
         MVC   ERRCODE,=H'307'                                                  
*                                                                               
BUYH0060 EQU   *                                                                
         XC    RDARKSRT,RDARKSRT   SET BUY HDR SUBRECORD TYPE                   
         XCEFL IOAREA+27,3973                                                   
*                                  CLEAR IOAREA: LEAVE KEY SET UP               
         LA    RF,34               SET INITIAL RECORD LENGTH                    
         LA    RE,RDARBYLE         INSERT LENGTH OF 01 ELEMENT                  
         AR    RF,RE                                                            
         STCM  RF,3,RDARLEN        INSERT INTO RECORD                           
         MVI   RDARELEM,X'01'      SET ELEMENT CODE                             
         STC   RE,RDARELLN         INSERT ELEMENT LENGTH                        
         LA    R5,RDARELEM                                                      
         USING RDARBYEL,R5                                                      
*                                                                               
         MVC   RDARBYRO,PBHDROTN   INSERT ROTATION                              
         MVC   RDARBYRS,PBHDRSDT   INSERT ROTATION START DAY                    
         MVC   SVBYRO,RDARBYRO                                                  
         MVN   ROTSTART,RDARBYRS   SAVE INCASE OF ORBITS                        
         PACK  DUB(8),PBHDSTIM(4)                                               
*                                  CONVERT AND STORE BUY START TIME             
         CVB   RF,DUB                                                           
         STCM  RF,3,RDARBYST       2 CHARS OUTPUT                               
         OC    PBHDETIM,FOXZEROS   SET TO ZERO IF BINARY                        
         PACK  DUB(8),PBHDETIM(4)                                               
*                                  CONVERT AND STORE BUY END   TIME             
         CVB   RF,DUB                                                           
         STCM  RF,3,RDARBYET       2 CHARS OUTPUT                               
         PACK  DUB(8),PBHDTSLN(3)                                               
*                                  CONVERT AND STORE TOTL SPOT LEN              
         CVB   RF,DUB                                                           
         STCM  RF,3,RDARBYSL       2 CHARS OUTPUT                               
         MVC   RDARBYSU,PBHDLUNT   INSERT LENGTH OF UNITS                       
         MVC   RDARBYPN,PBHDPGNM   INSERT PROGRAM NAME                          
         PACK  DUB(8),PBHDCOST(9)                                               
*                                  CONVERT AND STORE COST, WITH CENTS           
         CVB   RF,DUB                                                           
         STCM  RF,15,RDARBYCO      4 CHARS OUTPUT                               
         MVC   RDARBYCQ,PBHDCSQL   INSERT COST QUALIFIER                        
         OC    PBHDTMSH,FOXZEROS   SET TO ZERO IF SPACE                         
         PACK  DUB(8),PBHDTMSH(3)                                               
*                                  CONVERT AND STORE TIME SHARE                 
         CVB   RF,DUB                                                           
         STCM  RF,3,RDARBYTS       2 CHARS OUTPUT                               
         MVC   DAILYFLG,PBHDSTYP   SAVE WEEKLY/DAILY FLAG                       
*                                                                               
         TM    DARFLAGS,DFXML      ADD X'10' ELEMENT IF XML                     
         BO    BUYH0065                                                         
*                                                                               
         CLC   PBHDMGBL,SPACESX    MAKEGOOD LINK??                              
         BE    BUYH0080                                                         
*                                                                               
BUYH0065 DS    0H                                                               
         XC    ELTAREA,ELTAREA                                                  
ELM      USING RDARRVEL,ELTAREA                                                 
         MVI   ELM.RDARRVCD,X'10'                                               
         MVI   ELM.RDARRVLN,RDARRVLE                                            
*&&DO                                                                           
         CLI   PBHDMGBL,C'B'       BONUS MAKEGOOD?                              
         BNE   BUYH0070                                                         
         OI    ELM.RDARRVFG,X'80'                                               
         B     BUYH0072                                                         
*&&                                                                             
BUYH0070 DS    0H                                                               
         TM    DARFLAGS,DFXML      IF XML, SAVE AGENCY BUY NUMBER               
         BO    BUYH0071                                                         
         PACK  DUB(8),PBHDMGBL(3)  CONVERT AND STORE TARGET BUY LINE            
         CVB   RF,DUB              FOR THIS AGENCY MAKEGOOD BUY                 
         LTR   RF,RF               SKIP IF NO LINK                              
         BZ    BUYH0080                                                         
         STC   RF,ELM.RDARRVBY                                                  
*                                                                               
BUYH0071 MVC   ELM.RDARLXML,ABUYNUM SAVE OFF XML AGENCY BUY NUMBER              
*                                                                               
         DROP  ELM                                                              
BUYH0072 BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
BUYH0080 EQU   *                                                                
*                                                                               
*   BUY HEADER IS NOT WRITTEN HERE.  IT IS CLEARED BY THE NEXT                  
*      RECORD IN.                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*   BUYDEM:   BUY DEMO VALUES                                                   
*                                                                               
BUYDEM   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PBUYDEMD,R4                                                      
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
         LA    R5,ELTAREA                                                       
         USING RDARBMEL,R5                                                      
         MVI   RDARBMCD,X'20'                                                   
         MVI   RDARBMLN,RDARBMLQ                                                
*                                                                               
         MVC   RDARBDM1(28),PBDMVAL                                             
*                                                                               
         BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
*   BUY DEMO IS NOT WRITTEN HERE.  IT IS CLEARED BY THE NEXT                    
*      RECORD IN.                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*   BUYORB:   BUY ORBIT                                                         
*                                                                               
BUYORB   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PBUYORBD,R4                                                      
*                                                                               
         LA    R2,IOAREA SET OUTPUT AREA DSECT                                  
         USING RDARREC,R2                                                       
*                                                                               
*                                                                               
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         CLC   INPROG,LASTREC      LAST RECORD WRITTEN OUT?                     
         BE    BUYO0040            YES                                          
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         MVI   RDARKSRT,X'10'      SET SUBRECORD TYPE TO 'ORBIT'                
         XCEFL RDARELEM,997        CLEAR RECORD LEAVING KEY                     
         LA    RF,38               SET NEW LENGTH = KEY + 01 ELEMENT            
         STCM  RF,3,RDARLEN        INSERT INTO KEY                              
         MVC   RDARELEM(2),=X'0104'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
BUYO0040 EQU   *                                                                
         XC    ELTAREA(100),ELTAREA                                             
*                                  CLEAR ELEMENT BUILD AREA                     
         LA    R5,ELTAREA                                                       
         USING RDAROEEL,R5         SET ORBIT DSECT                              
*                                                                               
         MVI   RDAROECD,2          INSERT ELEMENT CODE                          
         LA    RF,RDAROELE                                                      
         STC   RF,RDAROELN         INSERT ELEMENT LENGTH                        
         MVC   RDAROERO,PBORROTN   INSERT ORBIT ROTATION                        
         MVC   RDAROESD,PBORRSDT   INSERT ORBIT START DAY                       
         PACK  DUB(8),PBORSTIM(4)                                               
*                                  CONVERT AND STORE BUY START TIME             
         CVB   RF,DUB                                                           
         STCM  RF,3,RDAROEST       2 CHARS OUTPUT                               
         OC    PBORETIM,FOXZEROS   SET TO ZEROS IF SPACE                        
         PACK  DUB(8),PBORETIM(4)                                               
*                                  CONVERT AND STORE BUY END   TIME             
         CVB   RF,DUB                                                           
         STCM  RF,3,RDAROEET       2 CHARS OUTPUT                               
         MVC   RDAROEPP,PBORPGNM   INSERT PROGRAM NAME                          
*                                                                               
         BAS   RE,LOADELT          INSERT ELEMENT INTO RECORD                   
*                                                                               
*                                                                               
*   BUY ORBIT IS NOT WRITTEN HERE.  THERE MAY BE MORE BUY ORBITS                
*      TO BE INSERTED.  RECORD WILL BE OUTPUT WHEN EITHER A NEW                 
*      ORBIT ELEMENT EXCEEDS RECORD SIZE MAX, OR A NEW TYPE COMES               
*      IN, TRIGGERING A WRITE.                                                  
*                                                                               
*   NEED TO SORT BUYORB ELEMENTS BASED ON ROTATION START DAY                    
*                                                                               
         BAS   RE,SORTORB                                                       
*                                                                               
BUYOX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*   NEED TO SORT BUYORB ELEMENTS BASED ON ROTATION START DAY                    
*                                                                               
SORTORB  NTR1                                                                   
         LA    R2,IOAREA+34                                                     
         SR    R4,R4                                                            
         SR    R5,R5                                                            
*                                                                               
SRTORB10 DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    SRTORB40                                                         
         CLI   0(R2),X'02'                                                      
         BL    SRTORB20                                                         
         BH    SRTORB40                                                         
*                                                                               
         AHI   R5,1                NUMBER OF ORBIT RECORDS                      
         LTR   R4,R4               POINT TO START OF ORBIT ELEMENTS             
         BNZ   SRTORB20                                                         
         LR    R4,R2                                                            
*                                                                               
SRTORB20 DS    0H                                                               
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     SRTORB10                                                         
*                                                                               
* INITIAL SORT WILL SORT ROTATION ELEMENTS BY MON-SUN                           
*                                                                               
SRTORB40 DS    0H                                                               
         CHI   R5,1                                                             
         BNH   SRTORBX                                                          
         GOTO1 =V(XSORT),DMCB,(1,(R4)),(R5),RDAROELE,7,2,RR=RELO                
*                                                                               
* SECOND SORT WILL SORT ROTATION ELEMENTS BY ROTATION START DAY                 
* THIS WILL PUT THE ORBITS WITH SPECIFIED START DAY IN CORRECT                  
* SEQUENCE                                                                      
*                                                                               
         CLI   ROTSTART,1          SKIP IF START DAY IS MONDAY                  
         BNH   SRTORBX                                                          
         CLI   ROTSTART,7          BUT SHOULDN'T BE > SUNDAY!                   
         BH    SRTORBX                                                          
*                                                                               
         ZIC   RF,ROTSTART                                                      
         LA    RE,8                                                             
         SR    RE,RF               CALULATE NUMBER OF DAYS TO SORT              
         ST    RE,DMCB+12                                                       
*                                                                               
         LA    RF,1(RF)            CALCULATE DISPLACEMENT                       
         ST    RF,DMCB+16                                                       
*                                                                               
         GOTO1 =V(XSORT),DMCB,(1,(R4)),(R5),RDAROELE,,,RR=RELO                  
*                                                                               
SRTORBX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   BUYCOM:   BUY COMMENT                                                       
*                                                                               
BUYCOM   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PBUYCOMD,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         CLC   INPROG,LASTREC      LAST RECORD WRITTEN OUT?                     
         BE    BUYC0040            YES                                          
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         MVI   RDARKSRT,X'20'      SET SUBRECORD TYPE TO 'BUY COMM'             
         XCEFL IOAREA+27,3973                                                   
*                                  CLEAR IOAREA: LEAVE KEY SET UP               
         LA    RF,38               SET INITIAL RECORD LENGTH                    
         STCM  RF,3,RDARLEN        INSERT INTO RECORD                           
         MVC   RDARELEM(2),=X'0104'                                             
*                                  SET ELEMENT CODE/LENGTH                      
BUYC0040 EQU   *                                                                
         XC    ELTAREA(100),ELTAREA                                             
*                                  CLEAR WORKSPACE                              
         LA    R5,ELTAREA                                                       
         USING RDARCTEL,R5                                                      
         MVI   RDARCTCD,2          INSERT ELEMENT CODE                          
         LA    R6,PBCMTEXT+69      SCAN COMMENT FOR BLANKS                      
         LA    RF,70               LOOP CONTROL                                 
BUYC0080 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   BUYC0120            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,BUYC0080         LOOP THROUGH ALL                             
         MVI   RDARCTLN,4          BLANK LINE:  SET CONTROL TO 4                
         MVC   RDARCTCM(2),SPACESX INSERT TWO BLANKS                            
         B     BUYC0160                                                         
BUYC0120 EQU   *                                                                
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
*                                                                               
*   AGENCY CAN SEND UP TO 70 CHARACTER BUYS.  REP PERMITS ONLY 60               
*     CHARACTERS ON A BUY COMMENT.  COMMENTS IN EXCESS OF 60 CHARS              
*     ARE TRUNCATED TO FIT.                                                     
*   JAN26/95 - OVERRIDDEN:  TRUNCATION WILL OCCUR IN REDAR20, SO                
*     ENTIRE COMMENT WILL SHOW IN DARE.                                         
*                                                                               
*                                                                               
***>>>   LA    RE,59               MAX SIZE FOR REP COMMENTS = 60               
***>>>   CR    RF,RE               NEW INPUT VS REP MAX                         
***>>>   BNH   BUYC0140            ACCEPTABLE                                   
***>>>   LR    RF,RE               NOT ACCEPTABLE:  USE REP MAX                 
BUYC0140 EQU   *                                                                
         EX    RF,BUYC0950         MOVE COMMENT BY LENGTH                       
         LA    RF,3(RF)            RESTORE LENGTH + L(CONTROL)                  
         STC   RF,RDARCTLN         INSERT LENGTH INTO ELEMENT                   
BUYC0160 EQU   *                                                                
         BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
BUYC0950 MVC   RDARCTCM(0),PBCMTEXT                                             
*                                  LOAD COMMENT BY LENGTH                       
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*   BUYDTL:   BUY DETAIL                                                        
*                                                                               
BUYDTL   NTR1                                                                   
         GOTOR BUYDTL00            CODE MOVED FOR ADDRESSIBILITY                
         B     EXIT                                                             
*                                                                               
*   AGYTLR:                                                                     
*                                                                               
AGYTLR   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYTLRD,R4                                                      
*                                                                               
         LA    R2,IOAREA           SET OUTPUT AREA DSECT                        
         USING RDARREC,R2                                                       
*                                                                               
*                                                                               
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         CLC   INPROG,LASTREC      LAST RECORD WRITTEN OUT?                     
         BE    AGYT0040            YES                                          
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         MVI   RDARKRT,X'50'       SET RECORD TYPE TO 'TRAILER'                 
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SUBKEY VALUES                          
         XCEFL RDARELEM,997        CLEAR RECORD LEAVING KEY                     
         LA    RF,34               SET NEW LENGTH = KEY + 01 ELEMENT            
         LA    RE,RDARLTLR                                                      
         AR    RF,RE                                                            
         STCM  RF,3,RDARLEN        INSERT INTO KEY                              
         MVI   RDARELEM,X'01'      INSERT ELEMENT CODE/LENGTH                   
         STC   RE,RDARELLN         INSERT LENGTH OF ELEMENT                     
AGYT0040 EQU   *                                                                
         LA    R5,RDARELEM                                                      
         USING RDARELE9,R5                                                      
*                                                                               
         PACK  DUB(8),PATLSPTS(6)                                               
*                                  CONVERT AND STORE TOTAL SPOTS                
         CVB   RF,DUB                                                           
         STCM  RF,7,RDARTSPT      INSERT TOTAL SPOTS                            
         PACK  DUB(8),PATLTOTL(10)                                              
*                                  CONVERT AND STORE TOTAL $$                   
         MVC   RDARTDOL,DUB+2     INSERT TOTAL $$                               
*                                     STORED AS PACKED!!                        
         PACK  DUB(8),PATLNMRC(6)                                               
*                                  CONVERT AND STORE TOTAL RECORDS              
         CVB   RF,DUB                                                           
         STCM  RF,7,RDAR#REC      INSERT TOTAL RECORDS                          
*                                                                               
         CLC   RDARTSPT,GROSPOTS+1 DETAIL SPOTS = TRAILER?                      
         BE    AGYT0060                                                         
         PRINT GEN                                                              
*                                                                               
* IF XML, INSERT TOTAL SPOTS AS ESPERANTO DOES NOT CALCULATE THIS               
*                                                                               
         TM    DARFLAGS,DFXML      XML?                                         
         BZ    AGYT0050                                                         
         MVC   RDARTSPT,GROSPOTS+1                                              
         B     AGYT0060                                                         
*                                                                               
AGYT0050 EQU   *                                                                
         GOTO1 =A(SNDERROR),DMCB,301,RR=RELO                                    
         PRINT NOGEN                                                            
         B     AGYT0100            NOW PUT OUT RECORD                           
AGYT0060 EQU   *                                                                
         XC    DUB,DUB             CLEAR ACCUMULATOR                            
         MVC   DUB+2(6),RDARTDOL   LOAD TOTAL DOLLARS                           
*                                                                               
* BYPASS CVB OVERFLOW DUMP                                                      
*                                                                               
         CP    DUB,=P'2147483647'                                               
         BH    AGYT0100                                                         
*                                                                               
         CVB   RF,DUB              CONVERT PACKED TO BINARY                     
         C     RF,GROSDOLS         DETAIL DOLLARS = TRAILER?                    
         BE    AGYT0100            YES                                          
*                                  NO  - SEND ERROR MESSAGE                     
*                                                                               
* IF XML, INSERT TOTAL $S AS ESPERANTO DOES NOT CALCULATE THIS                  
*                                                                               
         TM    DARFLAGS,DFXML      XML?                                         
         BZ    AGYT0090                                                         
         L     R0,GROSDOLS                                                      
         CVD   R0,DUB                                                           
         ZAP   RDARTDOL,=P'0'                                                   
         AP    RDARTDOL,DUB                                                     
         B     AGYT0100                                                         
*                                                                               
AGYT0090 EQU   *                                                                
         GOTO1 =A(SNDERROR),DMCB,302,RR=RELO                                    
AGYT0100 EQU   *                                                                
         BAS   RE,RECWRITE         OUTPUT THE RECORD                            
*                                                                               
*                                  RECORD AUDIT TRAIL                           
         GOTOR AUDTRAIL                                                         
*                                                                               
         CLI   OLDMEDI,C'R'                                                     
         BNE   AGYT0150            TV - SKIP COMPARING                          
*                                                                               
         GOTOR COMPARE                                                          
         BNE   AGYTX               PAS KEYS ADDED ALREADY                       
*                                  51S GOT CONVERTED TO 41S ALREADY             
*                                                                               
AGYT0150 DS    0H                                                               
*                                                                               
         GOTOR DUMPBUFF                                                         
*                                  ADD PASSIVE POINTERS TO DISK                 
         L     R4,APASAREA                                                      
         LA    R4,800(R4)          R4->NEW PASSIVE POINTERS                     
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',VCOMFACS),APASAREA,(R4),X        
               HDRDA                                                            
* DELETE ALL PASSIVE PTRS FOR 51 RECORDS                                        
         GOTOR DELPAS                                                           
* DO AUDIT TRAIL                                                                
         GOTOR DOAUDIT                                                          
*                                                                               
AGYTX    DS    0H                                                               
         TM    DARFLAGS,DFXML                                                   
         BZ    AGYTX10                                                          
         MVC   ERRCODE,=C'XX'      SEND OK TO XML PARSER                        
*                                                                               
AGYTX10  CLI   BUYHIFLG,C'Y'       IS BUY NUMBER TOO HIGH?                      
         BE    AGYTX20                                                          
         CLC   BUYCOUNT,=H'254'                                                 
         BNH   EXIT                                                             
         MVC   ERRCODE,=H'307'                                                  
AGYTX20  BRAS  RE,MARKREJ          MARK ORDER AS REJECTED                       
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R5                                                               
***********************************************************************         
*   AGYCAN:  AGENCY CANCEL RECORD                                               
*        THIS CAN BE 'NOTDARE' OR 'REVISION CANCEL' SITUATION                   
***********************************************************************         
AGYCAN   NTR1                                                                   
         GOTOR AGYCAN00                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   AGYHIA:  AGENCY HIATUS RECORD BECOMES AN X'15' TYPE RECORD                  
*        WITH A STRING OF DATES AS MAIN DATA.                                   
*                                                                               
AGYHIA   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYHIAD,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         LA    R5,RDARELEM+4       SET START OF X'02' ELEMENT                   
         USING RDARHIE2,R5                                                      
*                                                                               
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         CLC   INPROG,LASTREC      LAST RECORD WRITTEN OUT?                     
         BE    AGYH0040            YES                                          
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         MVI   RDARKRT,X'15'       SET REC TYPE TO 'HIATUS COMMENT'             
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR LOWER KEY                              
         XCEFL IOAREA+27,3973                                                   
*                                  CLEAR IOAREA: LEAVE KEY SET UP               
         LA    RF,40               SET INITIAL RECORD LENGTH                    
         STCM  RF,3,RDARLEN        INSERT INTO RECORD                           
         MVC   RDARELEM(2),=X'0104' SET ELEMENT CODE/LENGTH                     
         MVC   RDARHIC2(2),=X'0202' SET ELEMENT CODE/LENGTH                     
         LA    RF,RDARHIDT         SET A(1ST DATE IN ELEMENT)                   
         ST    RF,AHIATUS          SAVE A(1ST/NEXT DATE SLOT)                   
AGYH0040 EQU   *                                                                
         L     R6,AHIATUS          SET A(NEXT DATE SLOT)                        
         LA    R3,PAHIWKDT         SET A(1ST DATE OF INPUT)                     
         LA    R7,10               SET LOOP CONTROL                             
AGYH0060 EQU   *                                                                
         CLC   0(6,R3),SPACESX     ANYTHING IN FIELD?                           
         BE    AGYH0200            NO  - FINISHED WITH RECORD                   
         GOTO1 VDATCON,DMCB,(R3),(2,(R6))                                       
*                                  YES - CONVERT TO COMPRESSED FORMAT           
         CLC   0(2,R6),HIASTART    DATE PRIOR TO FLIGHT START?                  
         BL    AGYH0080            YES - DON'T OUTPUT IT                        
         CLC   0(2,R6),HIAEND      DATE AFTER FLIGHT END?                       
         BH    AGYH0080            YES - DON'T OUTPUT IT                        
         ZICM  RF,RDARLEN,2        RETRIEVE RECORD LENGTH                       
         LA    RF,2(RF)            INCREASE BY LENGTH OF DATE                   
         STCM  RF,3,RDARLEN        REPLACE IN RECORD                            
         LA    R6,2(R6)            BUMP TO NEXT AVAILABLE SLOT                  
         ST    R6,AHIATUS          STORE FOR NEXT RECORD (IF ANY)               
         ZIC   RF,RDARHIL2         INCREASE ELEMENT LENGTH                      
         LA    RF,2(RF)            BUMP BY L(NEW DATE)                          
         STC   RF,RDARHIL2         REPLACE IN ELEMENT                           
AGYH0080 EQU   *                                                                
         LA    R3,6(R3)            BUMP TO NEXT DATE FIELD                      
         BCT   R7,AGYH0060         GO BACK FOR NEXT                             
AGYH0200 EQU   *                                                                
         B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*   VARPRD:  EACH VARIOUS PRODUCT RECORD BECOMES A X'35' TYPE RECORD            
*        WITH ONE OR TWO BRANDS ATTACHED TO EACH ONE                            
*                                                                               
VARPRD   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYPROD,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   LASTREC,6(R3)       SET LAST RECORD TYPE                         
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD ALWAYS                 
         MVI   RDARKRT,X'35'       SET REC TYPE TO 'VARIOUS PRODUCT'            
*                                                                               
         ZIC   RF,RDARKSEQ         BRAND SEQUENCE                               
         LA    RF,1(RF)                                                         
         STC   RF,RDARKSEQ                                                      
*                                                                               
         MVI   RDARKSRT,0                                                       
*                                  CLEAR LOWER KEY                              
         XCEFL IOAREA+27,3973                                                   
*                                  CLEAR IOAREA: LEAVE KEY SET UP               
*                                  SET INITIAL RECORD LENGTH                    
         LA    RF,RDARPDLQ+34      INSERT INTO RECORD                           
         STCM  RF,3,RDARLEN                                                     
         MVI   RDARPDCD,X'01'      SET ELEMENT CODE/LENGTH                      
         MVI   RDARPDLN,RDARPDLQ   SET ELEMENT CODE/LENGTH                      
         MVC   RDARPDC1,PAPDCOD1   PRODUCT 1 CODE/NAME                          
         MVC   RDARPDN1,PAPDNAM1                                                
         MVC   RDARPDC2,PAPDCOD2   PRODUCT 2 CODE/NAME                          
         MVC   RDARPDN2,PAPDNAM2                                                
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*   MKGAPP:  MAKEGOOD APPROVAL RECORD                                           
*                                                                               
MKGAPP   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRAPPD,R4                                                      
*                                                                               
         MVC   QREPCON,MOAPRPCN    SAVE REP CONTRACT NUMBER                     
         MVC   QRETURN,MOAPRTNS    SAVE RETURN DATA                             
         MVC   ERRORD#,MOAPORDR    SAVE ORDER NUMBER FOR ERRORS                 
         MVC   ERRFRID,MOAPFRID    SAVE FROM ID FOR ERRORS                      
         MVC   ERRTOID,MOAPTOID    SAVE TO   ID FOR ERRORS                      
         MVC   ERRRCDT,MOAPDATE    SAVE RECEIVED DATE FOR ERRORS                
         MVC   ERRRCTM,MOAPTIME    SAVE RECEIVED TIME FOR ERRORS                
         MVC   ERRMGID,MOAPOFRI    SAVE MAKEGOOD ID FOR ERRORS                  
         MVC   ERRVER#,MOAPSEQN    SAVE MAKEGOOD VER# FOR ERRORS                
*                                                                               
         CLC   =C'FN',POWERCDE     DESTINATION TO FTS?                          
         BNE   *+14                                                             
         MVC   ERRCODE,=H'5'       SET ERROR, UNKNOWN DEST ID                   
         B     EXIT                                                             
*                                                                               
         LA    R2,IOAREA                                                        
         USING RCONRECD,R2                                                      
*                                                                               
         XC    RCONKEY,RCONKEY     CLEAR CONTRACT KEY AREA                      
         MVI   RCONPTYP,X'8C'      SET KEY TYPE 8C                              
         MVC   RCONPREP,POWERCDE   INSERT REP ID                                
         GOTO1 VHEXIN,DMCB,MOAPRPCN,WORK+32,8,=C'TOG'                           
*                                  COMPLEMENT CONTRACT NUMBER                   
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),WORK+32(4)                                            
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   RCONPCON+0(4),WORK+15                                            
*                                  INSERT CONTRACT NUMBER, 9'S COMP             
*                                     USED AGAIN FOR REVERSED.                  
         MVC   KEY(27),RCONKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BE    MGAP0040            YES -                                        
         MVC   ERRCODE,=H'908'     SET ERROR, CONTRACT NOT FOUND                
         B     MGAPX                                                            
*                                                                               
MGAP0040 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
         MVC   MGOFF,RCONKOFF      SAVE OFFICE OF ORDER                         
         MVC   MGSTA,RCONKSTA      SAVE STATION OF ORDER                        
         DROP  R2                                                               
*                                                                               
         XC    SVSSID,SVSSID                                                    
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'20'        GET SEND ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   MGAP0045                                                         
         USING RCONSEND,R6                                                      
         MVC   SVSSID,RCONSSID     GET REP ID INCASE MAKEGOOD HAS BEEN          
         DROP  R6                  SELF-APPLIED                                 
*                                                                               
MGAP0045 EQU   *                                                                
         LA    R2,IOAREA                                                        
         USING RMKGRECD,R2                                                      
*                                                                               
         XC    RMKGKEY,RMKGKEY     CLEAR THE DARE KEY AREA                      
         MVI   RMKGKTYP,X'11'      SET KEY TYPE 11                              
         MVC   RMKGKREP,POWERCDE   INSERT REP ID                                
         MVC   RMKGKOFF,MGOFF      INSERT OFFICE CODE                           
         MVC   RMKGKSTA,MGSTA      INSERT STATION                               
         CLI   RMKGKSTA+4,C'T'     TV STATION?                                  
         BNE   MGAP0050            NO                                           
         MVI   RMKGKSTA+4,C' '     YES - SPACE-FILL MEDIA                       
MGAP0050 EQU   *                                                                
         MVC   RMKGKCON+0(4),WORK+15                                            
*                                  REVERSE THE COMPLEMENT                       
         PACK  RMKGKCON+0(1),WORK+18(1)                                         
         PACK  RMKGKCON+1(1),WORK+17(1)                                         
         PACK  RMKGKCON+2(1),WORK+16(1)                                         
         PACK  RMKGKCON+3(1),WORK+15(1)                                         
         MVC   RMKGKGRP(2),MOAPOFRI                                             
*                                  INSERT GROUP NUMBER INTO ID                  
         MVC   KEY(27),RMKGKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?  (GROUP COMMENT)                   
         BE    MGAP0080            YES -                                        
*                                                                               
         CLC   MOAPOLDS,SPACESX    ANY OLD CALL LETTER?                         
         BNE   MGAP0055                                                         
         MVC   ERRCODE,=H'303'     SET ERROR, MAKEGOOD NOT FOUND                
         B     MGAPX                                                            
*                                                                               
MGAP0055 EQU   *                                                                
         MVC   RMKGKSTA,MOAPOLDS   INSERT OLD STATION                           
         CLI   RMKGKSTA+4,C'T'     TV STATION?                                  
         BNE   MGAP0060            NO                                           
         MVI   RMKGKSTA+4,C' '     YES - SPACE-FILL MEDIA                       
*                                                                               
MGAP0060 EQU   *                                                                
         MVC   KEY(27),RMKGKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?  (GROUP COMMENT)                   
         BE    MGAP0080            YES -                                        
         MVC   ERRCODE,=H'303'     SET ERROR, MAKEGOOD NOT FOUND                
         B     MGAPX                                                            
*                                                                               
MGAP0080 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
* NEW RULE: OFFER CAN BE SELF-APPLIED BY THE REP. THIS CONDITION WILL           
* BE CHECKED FIRST. IF OFFER IS INDEED SELF-APPLIED, WE WILL SET                
* ERRCODE TO BE C'SA' OR 58049 TO AUTO GENERATE A MKGROK BACK TO THE            
* AGENCY.                                                                       
*                                                                               
         TM    RMKGSFG1,RMGF1MCF   OFFER APPLIED?                               
         BZ    MGAP0082                                                         
         TM    RMKGSFG3,RMGF3SAQ   VIA A SELF-APPLY?                            
         BZ    MGAP0082                                                         
*                                                                               
         PACK  DUB(8),MOAPSEQN(2)  PACK VERSION NUMBER                          
         CVB   RF,DUB                                                           
         STC   RF,BYTE             SAVE VERSION NUMBER                          
         CLC   RMKGSCVR,BYTE       VERSIONS MUST AGREE!                         
         BNE   MGAP0090                                                         
*                                                                               
         OI    RMKGSFG3,RMGF3ARQ   MARK APPROVAL RECEIVED                       
         MVC   ERRCODE,=C'SA'      SET ERROR TO SEND MKGROK LATER               
         B     MGAP0090                                                         
*                                                                               
* IT IS POSSIBLE THAT THE AGENCY CAN APPROVE A MAKEGOOD THE SAME TIME           
* THE REP RESENDS. IN THIS CASE, ALWAYS CHECK IF THE APPROVAL IS MARKED         
* AGAINST THE CURRENT VERSION OF THE MAKEGOOD                                   
*                                                                               
* THE CURRENT DARE STATUS SHOULD BE SENT OR RESENT FOR AN APPROVAL TO           
* BE VALID                                                                      
MGAP0082 EQU   *                                                                
         TM    RMKGSFG1,RMGF1MSN+RMGF1MCR                                       
         BNZ   MGAP0085                                                         
         TM    RMKGSFG1,RMGF1MER   ERROR WAS RECEIVED, AND IT OVERWROTE         
         BZ    MGAP0083            THE LAST STATUS                              
         TM    RMKGSCST,RMKGSRCQ+RMKGSCNQ                                       
         BZ    MGAP0085            INSTEAD, CHECK CURRENT GROUP STATUS          
*                                                                               
MGAP0083 EQU   *                   DON'T NEED TO SEND ERROR                     
*                                  SINCE RECALL OVERRIDES APPROVAL              
*        MVC   ERRCODE,=H'926'     SET ERROR, MKGD RECALLED/CANCELLED           
         B     MGAPX                                                            
*                                                                               
MGAP0085 EQU   *                                                                
         PACK  DUB(8),MOAPSEQN(2)  PACK VERSION NUMBER                          
         CVB   RF,DUB                                                           
         STC   RF,BYTE             SAVE VERSION NUMBER                          
         CLC   RMKGSCVR,BYTE       VERSIONS MUST AGREE!                         
         BNE   MGAP0090            IF NOT, AUDIT IT AND EXIT                    
*                                                                               
         MVI   RMKGSFG1,X'20'      SET FLAG TO 'APPROVAL RECVD'                 
         DROP  R2                     SINGLE FLAG ONLY SET                      
*                                                                               
MGAP0090 EQU   *                                                                
         LA    R2,MGAUDELT                                                      
         USING RMKGATEM,R2                                                      
         XC    MGAUDELT,MGAUDELT                                                
*                                                                               
         MVI   RMKGATCD,X'02'                                                   
         MVI   RMKGATLN,RMKGATLQ                                                
*                                                                               
         PACK  DUB(8),MOAPSEQN(2)  PACK VERSION NUMBER                          
         CVB   RF,DUB                                                           
         STC   RF,RMKGATVR         SAVE VERSION NUMBER                          
         MVI   RMKGATAT,X'20'      SET AUDIT TRACK ELT                          
         GOTO1 VDATCON,DMCB,(5,0),(2,RMKGATDT)                                  
         BAS   RE,GETTPWOS         GET CURRENT TIME IN HHMMSS                   
         MVC   RMKGATTM,HHMMSS                                                  
         DROP  R2                                                               
*                                  INSERT TIME                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,MGAUDELT                   
*                                                                               
* SELF-APPLY OFFER? IF SO, RECORD AUTO MKGROK RESPONSE                          
*                                                                               
         CLC   =C'SA',ERRCODE                                                   
         BNE   MGAP0200                                                         
*                                                                               
         LA    R2,MGAUDELT                                                      
         USING RMKGATEM,R2                                                      
         XC    MGAUDELT,MGAUDELT                                                
*                                                                               
         MVI   RMKGATCD,X'02'                                                   
         MVI   RMKGATLN,RMKGAL2Q                                                
*                                                                               
         PACK  DUB(8),MOAPSEQN(2)  PACK VERSION NUMBER                          
         CVB   RF,DUB                                                           
         STC   RF,RMKGATVR         SAVE VERSION NUMBER                          
         MVI   RMKGATAT,X'08'      SET AUDIT TRACK ELT                          
         OI    RMKGATFG,X'40'      SET AUTO MKGROK GENERATED                    
         GOTO1 VDATCON,DMCB,(5,0),(2,RMKGATDT)                                  
*        BAS   RE,GETTPWOS         GET CURRENT TIME IN HHMMSS                   
*                                                                               
* SEPARATE THIS AUDIT HISTORY FROM PREVIOUS BY ADDING 1 SECOND                  
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,HHMMSS                                                      
*                                                                               
         MVC   RMKGATTM(1),HHMMSS                                               
         MVC   DUB+4(4),DUB                                                     
*                                                                               
         ICM   R1,15,DUB+4                                                      
         SLL   R1,20               SHIFT OFF HOURS AND MINUTES                  
         SRL   R1,20                                                            
         ST    R1,DUB                                                           
         AP    DUB(4),=PL4'1'      CHECK IF ADDING 1 SECOND                     
         CLC   DUB(4),=PL4'60'     WILL CAUSE A MINUTE CHANGE                   
         BL    MGAP0100                                                         
*                                                                               
         ICM   R1,15,DUB+4                                                      
         SLL   R1,12               SHIFT OFF HOURS                              
         SRL   R1,12                                                            
         ST    R1,DUB                                                           
         AP    DUB(4),=PL4'100'    CHECK IF ADDING 1 MINUTE                     
         CLC   DUB(4),=PL4'6000'   WILL CAUSE AN HOUR CHANGE                    
         BL    MGAP0110                                                         
*                                                                               
         MVC   DUB(4),DUB+4                                                     
         AP    DUB(4),=PL4'10000'                                               
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,4,RMKGATTM                                                    
         B     MGAP0150                                                         
*                                                                               
MGAP0100 EQU   *                                                                
         MVC   RMKGATTM+1(1),HHMMSS+1                                           
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STC   R1,RMKGATTM+2                                                    
         B     MGAP0150                                                         
*                                                                               
MGAP0110 EQU   *                                                                
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SIGN AND SECONDS                   
         STC   R1,RMKGATTM+1                                                    
         B     MGAP0150                                                         
         DROP  R2,R4                                                            
*                                                                               
MGAP0150 EQU   *                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,MGAUDELT                   
*                                                                               
MGAP0200 EQU   *                                                                
         BAS   RE,RECWRITE         REWRITE THE RECORD                           
         GOTO1 =A(UPDTRIS),RR=RELO UPDATE RIS PASSIVE KEYS                      
MGAPX    EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   MKGREJ:  MAKEGOOD REJECTION RECORD                                          
*                                                                               
MKGREJ   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRREJD,R4                                                      
*                                                                               
         MVC   QREPCON,MORJRPCN    SAVE REP CONTRACT NUMBER                     
         MVC   QRETURN,MORJRTNS    SAVE RETURN DATA                             
         MVC   ERRORD#,MORJORDR    SAVE ORDER NUMBER FOR ERRORS                 
         MVC   ERRFRID,MORJFRID    SAVE FROM ID FOR ERRORS                      
         MVC   ERRTOID,MORJTOID    SAVE TO   ID FOR ERRORS                      
         MVC   ERRRCDT,MORJDATE    SAVE RECEIVED DATE FOR ERRORS                
         MVC   ERRRCTM,MORJTIME    SAVE RECEIVED TIME FOR ERRORS                
         MVC   ERRMGID,MORJOFRI    SAVE MAKEGOOD ID FOR ERRORS                  
         MVC   ERRVER#,MORJSEQN    SAVE MAKEGOOD VER# FOR ERRORS                
*                                                                               
         CLC   =C'FN',POWERCDE     DESTINATION TO FTS?                          
         BNE   *+14                                                             
         MVC   ERRCODE,=H'5'       SET ERROR, UNKNOWN DEST ID                   
         B     EXIT                                                             
*                                                                               
         LA    R2,IOAREA                                                        
         USING RCONRECD,R2                                                      
*                                                                               
         XC    RCONKEY,RCONKEY     CLEAR CONTRACT KEY AREA                      
         MVI   RCONPTYP,X'8C'      SET KEY TYPE 8C                              
         MVC   RCONPREP,POWERCDE   INSERT REP ID                                
         GOTO1 VHEXIN,DMCB,MORJRPCN,WORK+32,8,=C'TOG'                           
*                                  COMPLEMENT CONTRACT NUMBER                   
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),WORK+32(4)                                            
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   RCONPCON+0(4),WORK+15                                            
*                                  INSERT CONTRACT NUMBER, 9'S COMP             
*                                     USED AGAIN FOR REVERSED.                  
         MVC   KEY(27),RCONKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BE    MGRJ0040            YES -                                        
         MVC   ERRCODE,=H'908'     SET ERROR, CONTRACT NOT FOUND                
         B     MGRJX                                                            
MGRJ0040 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
         MVC   MGOFF,RCONKOFF      SAVE OFFICE OF ORDER                         
         MVC   MGSTA,RCONKSTA      SAVE STATION OF ORDER                        
         LA    R2,IOAREA                                                        
         USING RMKGRECD,R2                                                      
*                                                                               
         XC    RMKGKEY,RMKGKEY     CLEAR THE DARE KEY AREA                      
         MVI   RMKGKTYP,X'11'      SET KEY TYPE 11                              
         MVC   RMKGKREP,POWERCDE   INSERT REP ID                                
         MVC   RMKGKOFF,MGOFF      INSERT OFFICE CODE                           
         MVC   RMKGKSTA,MGSTA      INSERT STATION                               
         CLI   RMKGKSTA+4,C'T'     TV STATION?                                  
         BNE   MGRJ0050            NO                                           
         MVI   RMKGKSTA+4,C' '     YES - SPACE-FILL MEDIA                       
MGRJ0050 EQU   *                                                                
         MVC   RMKGKCON+0(4),WORK+15                                            
*                                  REVERSE THE COMPLEMENT                       
         PACK  RMKGKCON+0(1),WORK+18(1)                                         
         PACK  RMKGKCON+1(1),WORK+17(1)                                         
         PACK  RMKGKCON+2(1),WORK+16(1)                                         
         PACK  RMKGKCON+3(1),WORK+15(1)                                         
         MVC   RMKGKGRP(2),MORJOFRI                                             
*                                  INSERT GROUP NUMBER INTO ID                  
         MVC   KEY(27),RMKGKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?  (GROUP COMMENT)                   
         BE    MGRJ0080            YES -                                        
*                                                                               
         CLC   MORJOLDS,SPACESX    ANY OLD CALL LETTER?                         
         BNE   MGRJ0055                                                         
         MVC   ERRCODE,=H'303'     SET ERROR, MAKEGOOD NOT FOUND                
         B     MGRJX                                                            
*                                                                               
MGRJ0055 EQU   *                                                                
         MVC   RMKGKSTA,MORJOLDS   INSERT OLD STATION                           
         CLI   RMKGKSTA+4,C'T'     TV STATION?                                  
         BNE   MGRJ0060            NO                                           
         MVI   RMKGKSTA+4,C' '     YES - SPACE-FILL MEDIA                       
*                                                                               
MGRJ0060 EQU   *                                                                
         MVC   KEY(27),RMKGKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?  (GROUP COMMENT)                   
         BE    MGRJ0080            YES -                                        
         MVC   ERRCODE,=H'303'     SET ERROR, MAKEGOOD NOT FOUND                
         B     MGRJX                                                            
*                                                                               
MGRJ0080 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
         MVI   MGINPROG,C'Y'       SET MG REJECT IN PROGRESS                    
*                                                                               
* IT IS POSSIBLE THAT THE AGENCY CAN REJCT A MAKEGOOD THE SAME TIME             
* THE REP RESENDS. IN THIS CASE, ALWAYS CHECK IF THE APPROVAL IS MARKED         
* AGAINST THE CURRENT VERSION OF THE MAKEGOOD                                   
*                                                                               
         PACK  DUB(8),MORJSEQN(2)  PACK VERSION NUMBER                          
         CVB   RF,DUB                                                           
         STC   RF,BYTE             SAVE VERSION NUMBER                          
         CLC   RMKGSCVR,BYTE       VERSIONS MUST AGREE!                         
         BNE   MGRJ0090                                                         
*                                                                               
         MVI   RMKGSFG1,X'10'      SET FLAG TO 'REJECTION RECVD'                
         DROP  R2                                                               
*                                                                               
MGRJ0090 EQU   *                                                                
         LA    R2,MGAUDELT                                                      
         USING RMKGATEM,R2                                                      
         XC    MGAUDELT,MGAUDELT                                                
*                                                                               
         MVI   RMKGATCD,X'02'                                                   
         MVI   RMKGATLN,RMKGATLQ                                                
*                                                                               
         PACK  DUB(8),MORJSEQN(2)  PACK VERSION NUMBER                          
         CVB   RF,DUB                                                           
         STC   RF,RMKGATVR         SAVE VERSION NUMBER                          
         MVI   RMKGATAT,X'10'      SET AUDIT ACTION                             
         GOTO1 VDATCON,DMCB,(5,0),(2,RMKGATDT)                                  
         BAS   RE,GETTPWOS         GET CURRENT TIME IN HHMMSS                   
         MVC   RMKGATTM,HHMMSS                                                  
         DROP  R2,R4                                                            
*                                  INSERT TIME                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,MGAUDELT                   
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'05',IOAREA),0,0                
*                                  DELETE OLD COMMENT ELEMENTS                  
MGRJX    EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*   MKGRCM:  MAKEGOOD REJECTION COMMENT RECORD                                  
*                                                                               
MKGRCM   NTR1                                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRCOMD,R4                                                      
         LA    R2,IOAREA                                                        
         USING RMKGRECD,R2                                                      
*                                                                               
         CLI   MGINPROG,C'Y'       MG REJECT IN PROGRESS?                       
         BNE   MGCM0120            NO  - SKIP PROCESSING THIS RECORD            
         XC    ELTAREA,ELTAREA                                                  
         MVI   ELTAREA,5           INSERT ELEMENT CODE                          
         LA    RF,MORCTEXT+69      SET A(LAST POS OF TEXT IN COMMENT)           
         LA    RE,70               SET LOOP CONTROL                             
MGCM0020 EQU   *                                                                
         CLI   0(RF),C' '          POSITION CONTAINS SPACE?                     
         BNE   MGCM0040            NO  - REAL INFO FOUND                        
         BCTR  RF,0                BACK UP ONE POSITION                         
         BCT   RE,MGCM0020         GO BACK FOR NEXT                             
         LA    RE,1                ALL SPACES?  MOVE 1 OF THEM                  
MGCM0040 EQU   *                                                                
         BCTR  RE,0                SUBTRACT 1 FOR EX                            
         EX    RE,MGCM0200         MOVE BY LENGTH                               
         LA    RE,3(RE)            ADD FOR ELEMENT LENGTH                       
         STC   RE,ELTAREA+1        INSERT INTO ELEMENT                          
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA,          X        
               =C'ADD=CODE'                                                     
         XC    ELTAREA,ELTAREA                                                  
MGCM0120 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
MGCM0200 MVC   ELTAREA+2(0),MORCTEXT                                            
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*                                  FETCH TODAY'S DATE                           
TIMEFLAT NTR1                                                                   
*                                                                               
*   TIME INPUT:  COMES IN AS HH:MM:SS:TT.  THESE ARE THE DECIMAL                
*      EQUIVALENTS.  FOR EXAMPLE, 3:45:45:00PM COMES IN AS                      
*      07:45:45:00.  3PM IS REALLY 1500 HOURS, WHICH DDS SETS TO                
*      0700 HOURS.  THE MINUTES MAY BE USED AS REPRESENTED.  THE                
*      HOURS MUST BE CONVERTED TO ZONED DECIMAL, THEN PACKED AND                
*      CONVERTED TO BINARY, SO THAT 8 MAY BE ADDED TO THEM, THEN                
*      THE RESULTS RECONVERTED TO ZONED DECIMAL, STRUNG WITH THE                
*      MINUTES FIELD, THEN PACKED AND CONVERTED BACK TO BINARY FOR              
*      FINAL STORAGE.                                                           
*                                                                               
         TIME  DEC                                                              
*                                  RETRIEVE TIME:  RETURNED IN R0               
*                                     AS HH:MM:SS:TT                            
         XC    WORK,WORK           CLEAR WORK SPACE                             
         ST    R0,SAVTIME          SAVE TIME                                    
         MVC   WORK(1),SAVTIME+1   MOVE MM IN WORK FIELD                        
         GOTO1 VHEXOUT,DMCB,WORK,WORK+34,1,=C'TOG'                              
*                                  CONVERT MINS TO ZONED DECLS                  
         MVC   WORK(1),SAVTIME     MOVE HH TO WORK FIELD                        
         GOTO1 VHEXOUT,DMCB,WORK,WORK+32,1,=C'TOG'                              
*                                  CONVERT HOURS TO ZONED DECLS                 
         PACK  DUB(8),WORK+32(2)                                                
*                                  PACK HH                                      
         CVB   R3,DUB                                                           
*                                  CONVERT RESULT TO BINARY                     
* SHOULD REALLY USE THMS, CHANGE THIS WHEN POSSIBLE...                          
         LA    R3,6(R3)            ADD 6 TO HOURS                               
*                                  (ADJUST DDS CLOCK TO REAL WORLD)             
         STC   R3,WORK             STORE HOURS IN WORK FIELD                    
         EDIT  (R3),(2,WORK+32),FILL=0,ZERO=NOBLANK                             
*                                  EDIT HH (BINARY) TO ZONED DECLS              
*                                                                               
         PACK  DUB(8),WORK+32(4)                                                
*                                  PACK HHMM                                    
         CVB   RF,DUB                                                           
*                                  CONVERT RESULT TO BINARY                     
         STCM  RF,3,SAVTIME        SAVE LAST TWO BYTES                          
         B     EXIT                                                             
*                                                                               
* THMS TIME ROUTINE TO GET PWOS TIME IN HHMMSS FORMAT                           
*                                                                               
GETTPWOS NTR1                                                                   
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,HHMMSS                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   LOADELT :  LOADS ELEMENT IN WORKSPACE (ELTAREA) TO RECORD                   
*        CHECKS RECORD SIZE TO SEE IF CURRENT RECORD MUST BE                    
*        WRITTEN, AND ANOTHER SET UP                                            
*                                                                               
LOADELT  NTR1                                                                   
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         ZICM  RF,RDARLEN,2        GET RECORD LENGTH                            
         ZIC   RE,ELTAREA+1        GET ELEMENT LENGTH                           
         AR    RF,RE               CALCULATE NEW LENGTH                         
         C     RF,=F'3972'         DOES RECORD EXCEED MAXIMUM?                  
LELT0010 BNH   LELT0080            NO  - ADD ELEMENT                            
         BAS   RE,RECWRITE         YES - WRITE OUT CURRENT RECORD               
*                                                                               
*   NEED TO KEEP 01 ELEMENT IN RECORD.  CLEAR BEYOND IT.                        
*                                                                               
         ZIC   RF,RDARELEM+1       GET LENGTH OF 01 ELEMENT                     
         LA    RF,34(RF)           ADD KEY LENGTH TO IT                         
         STCM  RF,3,RDARLEN        INSERT KEY+01 ELT LENGTH                     
         CLI   RDARKRT,X'40'       BUY RECORD?                                  
         BE    LELT0030            NO  -                                        
*                                                                               
         CLI   RDARKRT,X'15'       HIATUS COMMENT?                              
         BE    LELT0020                                                         
         CLI   RDARKRT,X'20'       STANDARD COMMENT?                            
         BE    LELT0020                                                         
         CLI   RDARKRT,X'30'       ORDER COMMENT?                               
         BNE   LELT0040                                                         
*                                                                               
LELT0020 EQU   *                                                                
         ZIC   RF,RDARKSEQ         YES - BUMP THE SEQUENCE TYPE                 
         AHI   RF,1                                                             
         STC   RF,RDARKSEQ         PUT IT BACK                                  
         B     LELT0060                                                         
*                                                                               
LELT0030 EQU   *                                                                
         ZIC   RF,RDARKSRT         YES - BUMP THE SUBRECORD TYPE                
         LA    RF,1(RF)                                                         
         STC   RF,RDARKSRT         PUT IT BACK                                  
         B     LELT0060                                                         
LELT0040 EQU   *                                                                
         ZIC   RF,RDARKRT          BUMP THE RECORD TYPE                         
         LA    RF,1(RF)                                                         
         STC   RF,RDARKRT          PUT IT BACK                                  
LELT0060 EQU   *                                                                
         ZIC   RF,RDARELEM+1       GET LENGTH OF 01 ELEMENT AGAIN               
         LA    RE,RDARELEM         A(01 ELEMENT)                                
         AR    RE,RF               GET A(FIRST AFTER 01 ELEMENT)                
         XC    0(250,RE),0(RE)     CLEAR OUT A REASONABLE AMOUNT                
*                                  ADD ELT TO REINITIALIZED RECORD              
LELT0080 EQU   *                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA,          X        
               =C'ADD=END'                                                      
*                                  ADD ELT TO RECORD                            
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*   RECWRITE:  CHECKS FOR KEYS, DELETED RECORDS, ETC....                        
*                                                                               
RECWRITE NTR1                                                                   
*** TESTING                                                                     
*        GOTOR ADDPPSON                                                         
*** TESTING                                                                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY(L'RDARKEY),IOAREA     SET KEY FROM NEW RECORD                
         GOTO1 VDATAMGR,DMCB,(X'88',ADDREC),=C'REPFILE',KEY,           X        
               IOAREA,IOWORK                                                    
*                                  ATTEMPT TO ADD NEW RECORD                    
         CLI   PAR3,0              ERROR RETURN?                                
         BZ    RECW0400            NO  - SUCCESSFUL                             
         CLI   PAR3,X'20'          YES - DUPE KEY?                              
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - CAN'T PROCESS: DUMP                    
*                                                                               
*                                  RESTORE THE KEY/OVERWRITE RECORD             
         MVC   KEY(L'RDARKEY),IOAREA       SET UP KEY AGAIN                     
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BE    *+6                 YES                                          
         DC    H'0'                ???                                          
         MVI   KEY+27,0            RESTORE KEY                                  
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,        X        
               AIO2,IOWORK                                                      
*                                  RETRIEVE INTO IO AREA # 2                    
         NI    IOAREA+29,X'FF'-X'80'                                            
*                                  RESTORE CONTROL IN RECORD                    
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMUNLK',=C'REPFILE'                             
*                                  REWRITE FROM NEW REC: IO AREA # 1            
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEYSAVE,KEY             
*                                  REWRITE CLEARED KEY                          
         B     RECW0500                                                         
RECW0400 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(27),IOAREA      SET KEY FROM NEW RECORD                      
*                                                                               
RECW0500 EQU   *                                                                
K        USING RDARKEY,KEY                                                      
         CLI   K.RDARKRT,X'10'     AGENCY ORDER HEADER?                         
         BNE   RECW0600            NO                                           
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BE    *+6                 YES                                          
         DC    H'0'                ???                                          
*                                                                               
         MVC   HDRDA,KEY+28        SAVE HEADER D/A                              
         L     R4,APASAREA         CREATE NEW PASSIVE POINTER                   
         LA    R4,800(R4)                                                       
         LA    R2,IOAREA                                                        
*                                  CLEAR FIRST POSITIONS OF WORK AREA           
         XC    0(128,R4),0(R4)                                                  
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',VCOMFACS),(R4),(R2),    X        
               ASPLAREA                                                         
*                                                                               
         DROP  K                                                                
*                                                                               
RECW0600 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*&&DO                                                                           
*                                                                               
*   LOADTABL:  TEMPORARY ROUTINE TO LOAD A TEMPORARY TABLE, AS IF IT            
*        WERE THE TABLE BEING PASSED FROM SRTIM.                                
*                                                                               
LOADTABL NTR1                                                                   
         XC    WORKTABL,WORKTABL   CLEAR TEMPORARY TABLE                        
         LA    R4,WORKTABL                                                      
         USING DREPTABL,R4                                                      
         MVI   DUMPCTR,0           CLEAR TEST COUNTERS                          
         MVI   TESTCTR,0                                                        
         CLC   RDRRD1+20(2),=C'  ' ANY COUNTER?                                 
         BE    LTAB0020            NO                                           
         CLC   RDRRD1+20(2),=X'0000'                                            
         BE    LTAB0020                                                         
         PACK  DUB(8),RDRRD1+20(2)                                              
         CVB   RF,DUB                                                           
         STC   RF,DUMPCTR          STORE COUNTER                                
LTAB0020 EQU   *                                                                
         LA    R2,RDRRD1H          A(FIRST LINE CONTROL HEADER)                 
LTAB0040 EQU   *                                                                
         CLI   5(R2),0             ANYTHING ON LINE?                            
         BZ    LTAB0120            NO  - FINISHED                               
         LR    R3,R2               SAVE VALUE OF R2                             
*                                                                               
         LA    R2,8(R2)            SKIP HEADER                                  
         CLC   =C'EJ',0(R2)        REQUEST FOR EJOR FILE?                       
         BNE   LTAB0060            NO                                           
         LH    RF,=X'0966'         YES - FORCE TO EJOR FILE                     
         B     LTAB0080                                                         
LTAB0060 EQU   *                                                                
         PACK  DUB(8),0(2,R2)      PACK ID NUMBER                               
         CVB   RF,DUB              CONVERT ID NUMBER TO BINARY                  
LTAB0080 EQU   *                                                                
         STCM  RF,3,DRUSERID       INSERT USER ID INTO TABLE                    
         MVC   DRSUBID,3(R2)       INSERT SUB ID INTO TABLE                     
         PACK  DUB(8),7(3,R2)      PACK ID NUMBER                               
         CVB   RF,DUB              CONVERT ID NUMBER TO BINARY                  
         STCM  RF,3,DRREPT#        INSERT REPORT NUMBER INTOTABLE               
         LR    R2,R3               RESET R2                                     
         LA    R2,RDRRD2H-RDRRD1H(R2)                                           
*                                  BUMP TO NEXT LINE                            
         LA    R4,LCORTAB(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     LTAB0040            GO BACK FOR NEXT                             
LTAB0120 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* SWITCHES SYSTEM WHETHER THE PROGRAM HAS BEEN AUTHORIZED TO SWITCH             
* SYSTEMS OR NOT                                                                
*                                                                               
* ON ENTRY:    DMCB    BYTE  0     SYSTEM SENUM TO SWITCH TO                    
***********************************************************************         
SWTCHSYS NTR1                                                                   
         MVC   DMCB+1(3),=3X'FF'   DON'T CARE IF PROGRAM IS AUTHORIZED          
         GOTO1 VSWITCH,DMCB,,0                                                  
         CLI   DMCB+4,0            SUCCESSFUL?                                  
         BNE   SWSYSNO             NO                                           
*                                                                               
SWSYSYES B     YES                                                              
*                                                                               
SWSYSNO  B     NO                                                               
*                                                                               
*   LOCAL VALUES                                                                
*                                                                               
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRITE  DC    CL8'DMWRT   '                                                    
DMADD    DC    CL8'DMADD   '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
FOXZEROS DC    20C'0'                                                           
SPACESX  DC    20C' '                                                           
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
EXITPRG  L     RD,SAVERD                                                        
         B     EXIT                                                             
         LTORG                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*   RECTABLE:  TABLE OF RECORD IDENTIFIERS FROM PRINT QUEUE RECORDS.            
*        WITH ASSOCIATED FORMATING ROUTINES, AND BREAK INDICATOR                
*        FLAGS.                                                                 
*                                                                               
*        COL 1-6 = RECORD NAME                                                  
*        COL 7   = RECORD TYPE                                                  
*        COL 8   = GROUP TYPE                                                   
*                  X'01' = AGENCY ORDER                                         
         DS    0F                  ALIGNMENT                                    
RECTABLE EQU   *                                                                
         DC    CL6'AGYHDR',XL1'01,01',AL4(AGYHDR),AL1(PAGYHDRL)                 
LRECTAB  EQU   *-RECTABLE          SIZE OF ENTRY                                
         DC    CL6'AGYDS1',XL1'01,01',AL4(AGYDS1),AL1(PAGYDS1L)                 
         DC    CL6'AGYDS2',XL1'01,01',AL4(AGYDS2),AL1(PAGYDS2L)                 
         DC    CL6'AGYDS3',XL1'01,01',AL4(AGYDS3),AL1(PAGYDS3L)                 
         DC    CL6'AGYDS4',XL1'01,01',AL4(AGYDS4),AL1(PAGYDS4L)                 
         DC    CL6'AGYDS5',XL1'01,01',AL4(AGYDS5),AL1(PAGYDS5L)                 
         DC    CL6'AGYEDI',XL1'01,01',AL4(AGYEDI),AL1(PAGYDS1L)                 
         DC    CL6'AGYSTD',XL1'02,01',AL4(AGYSTD),AL1(PAGYSTDL)                 
         DC    CL6'AGYCOM',XL1'03,01',AL4(AGYCOM),AL1(PAGYCOML)                 
         DC    CL6'BUYHDR',XL1'04,01',AL4(BUYHDR),AL1(PBUYHDRL)                 
         DC    CL6'BUYDEM',XL1'04,01',AL4(BUYDEM),AL1(PBUYDEML)                 
         DC    CL6'BUYORB',XL1'05,01',AL4(BUYORB),AL1(PBUYORBL)                 
         DC    CL6'BUYCOM',XL1'06,01',AL4(BUYCOM),AL1(PBUYCOML)                 
         DC    CL6'BUYDTL',XL1'07,01',AL4(BUYDTL),AL1(PBUYDTLL)                 
         DC    CL6'AGYTLR',XL1'08,01',AL4(AGYTLR),AL1(PAGYTLRL)                 
         DC    CL6'AGYCAN',XL1'09,00',AL4(AGYCAN),AL1(0)                        
         DC    CL6'AGYHIA',XL1'10,01',AL4(AGYHIA),AL1(PAGYHIAL)                 
         DC    CL6'MKGAPP',XL1'11,00',AL4(MKGAPP),AL1(0)                        
         DC    CL6'MKGREJ',XL1'12,00',AL4(MKGREJ),AL1(0)                        
         DC    CL6'MKGRCM',XL1'12,00',AL4(MKGRCM),AL1(0)                        
*                                                                               
* A VARIOUS ORDER IS EXACTLY THE SAME AS A REGULAR ORDER WITH THE               
* EXCEPTION OF THE VARPRD RECORD. ANY NEW ORDER RECORDS SHOULD BE               
* DUPLICATED FOR VARIOUS AS WELL                                                
*                                                                               
         DC    CL6'VARHDR',XL1'13,00',AL4(AGYHDR),AL1(0)                        
         DC    CL6'VARDS1',XL1'13,00',AL4(AGYDS1),AL1(0)                        
         DC    CL6'VARDS2',XL1'13,00',AL4(AGYDS2),AL1(0)                        
         DC    CL6'VARDS3',XL1'13,00',AL4(AGYDS3),AL1(0)                        
         DC    CL6'VARDS4',XL1'13,00',AL4(AGYDS4),AL1(0)                        
         DC    CL6'VAREDI',XL1'13,00',AL4(AGYEDI),AL1(0)                        
         DC    CL6'VARSTD',XL1'14,00',AL4(AGYSTD),AL1(0)                        
         DC    CL6'VARCOM',XL1'15,00',AL4(AGYCOM),AL1(0)                        
         DC    CL6'VARTLR',XL1'16,00',AL4(AGYTLR),AL1(0)                        
         DC    CL6'VARCAN',XL1'17,00',AL4(AGYCAN),AL1(0)                        
         DC    CL6'VARHIA',XL1'18,00',AL4(AGYHIA),AL1(0)                        
*                                                                               
         DC    CL6'VARPRD',XL1'19,00',AL4(VARPRD),AL1(0)                        
*                                                                               
         DC    XL2'0000'           DELIMITER                                    
         EJECT                                                                  
*&&DO                                                                           
*********************************************************                       
* ADD POINT PERSON (HARD CODE FOR TESTIRNG PURPOSE)                             
*********************************************************                       
ADDPPSON NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOAREA                                                        
         USING RDARREC,R2                                                       
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   ADDPPX              NO                                           
         CLI   RDARKSTA+4,C'A'     ONLY DO THIS FOR TV                          
         BE    ADDPPX                                                           
         CLI   RDARKSTA+4,C'F'                                                  
         BE    ADDPPX                                                           
*                                                                               
         LA    RF,RDARELEM         SET A(01 ELEMENT)                            
ADDP0080 EQU   *                                                                
         CLI   0(RF),X'00'         END OF RECORD?                               
         BE    ADDP0160            YES - ELEMENT(S) NOT FOUND                   
         CLI   0(RF),X'0A'         POINTPERSON ELEMENT?                         
         BE    ADDPPX              YES, THERE ALREADY, EXIT                     
ADDP0120 EQU   *                                                                
         ZIC   RE,1(RF)            ELEMENT LENGTH                               
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     ADDP0080            GO BACK FOR NEXT                             
*                                                                               
ADDP0160 DS    0H                                                               
         XC    ELTAREA(20),ELTAREA                                              
         MVC   ELTAREA(2),=XL2'0A07'                                            
         MVC   ELTAREA+2(5),=C'     '                                           
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA,          X        
               =C'ADD=END'                                                      
ADDPPX   DS    0H                                                               
         DROP  R2                                                               
         B     EXIT                                                             
*&&                                                                             
*                                                                               
*  RDPQREP:  RETRIEVE THE PRINT QUEUE ENTRY, AND SET THE INFO                   
*    FOR ERROR MESSAGE 309 - REP SYSTEM NOT UP                                  
*                                                                               
RDPQREP  NTR1  BASE=*,LABEL=*                                                   
         GOTOR RETRVREP,DMCB,(R2)  GET THE REPORT FROM PRINT QUEUE              
         JNE   NO                                                               
                                                                                
         XC    R,R                                                              
         LA    R4,PQINDEX          SET INDEX FOR REPORT                         
         USING UKRECD,R4                                                        
RDPQRP10 GOTO1 VDATAMGR,DMCB,(0,=C'READ'),UKUSRINF,0,R,APQREC,0                 
         CLI   DMCB+8,0            END OF PQ CONTROL INTERVAL?                  
         JNE   NO                                                               
                                                                                
         XC    ERRMGID,ERRMGID     MAKEGOOD ID FOR ERRORS                       
         XC    ERRVER#,ERRVER#     MAKEGOOD VER# FOR ERRORS                     
         CLC   =C'AGYHDR',R+1      FIND AGYHDR RECORD                           
         BNE   RDPQRP10                                                         
         LA    R5,R+1              SET INPUT RECORD DSECT                       
         USING PAGYHDRD,R5                                                      
         MVC   QREPCON,PAHDRPCN    SAVE REP CONTRACT NUMBER                     
         MVC   QRETURN,PAHDRTRN    SAVE RETURN DATA                             
         MVC   ERRORD#,PAHDORDR    SAVE ORDER NUMBER FOR ERRORS                 
         MVC   ERRFRID,PAHDFRID    SAVE FROM ID FOR ERRORS                      
         MVC   ERRTOID,PAHDTOID    SAVE TO   ID FOR ERRORS                      
         MVC   ERRRCDT,PAHDDATE    SAVE RECEIVED DATE FOR ERRORS                
         MVC   ERRRCTM,PAHDTIME    SAVE RECEIVED TIME FOR ERRORS                
         J     YES                                                              
                                                                                
         DROP  R4,R5                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  RETRVREP:  RETRIEVE THE PRINT QUEUE ENTRY, AND GENERATE THE                  
*    APPROPRIATE RECORDS ON THE REP FILE                                        
*                                                                               
RETRVREP NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            SET A(TABLE ENTRY)                           
         USING DREPTABL,R2                                                      
*                                                                               
         XC    PQINDEX,PQINDEX                                                  
         LA    R3,PQINDEX          SET INDEX FOR REPORT                         
         USING UKRECD,R3                                                        
         MVC   UKSRCID,DRUSERID                                                 
*                                  INSERT USER ID CODE                          
         MVC   UKSUBID,DRSUBID     INSERT SUBID                                 
         MVC   UKREPNO,DRREPT#                                                  
*                                  INSERT REPORT NUMBER INTO KEY                
*                                                                               
RETR0080 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(0,=C'GFILE'),=C'PRTQUE',PQINDEX,R,APQREC          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BAD GFILE CALL                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'INDEX'),UKUSRINF,PQINDEX,R,APQREC            
*                                  ACCESS REPORT                                
         CLI   8(R1),0                                                          
RETR0900 EQU   *                                                                
         J     EXIT                                                             
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETOLD:  DELETE OLD RECORDS, IF ANY, FOR THIS ORDER.  KEY                   
*        IS BUILT THROUGH ORDER NUMBER.                                         
*                                                                               
* NOTE: AGENCY ROUTING OFFICE MAY HAVE CHANGED. THEREFORE, WE NEED TO           
* READ ALL KEYS FOR STATION, AGENCY (PREFIX ONLY WITHOUT OFFICE CODE)           
* AND ORDER NUMBER TO MAKE SURE THAT ORDER DOES NOT EXISTS UNDER A              
* DIFFERENT OFFICE CODE.                                                        
***********************************************************************         
DELETOLD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SAVEMISC,0          CLEAR SAVE AREA                              
         NI    DARFLAGS,X'FF'-DFOFFCHG                                          
         LA    R2,IOAREA                                                        
         USING RDARREC,R2                                                       
*                                                                               
         XC    ORDARKEY,ORDARKEY   CLEAR ORIGINAL DARE KEY                      
         MVI   RESENTFL,C'N'       SET 'ORDER RESENT=NO'                        
*                                                                               
* CHECK EXACT KEY MATCH FIRST (AGENCY OFFICE HAS NOT BEEN CHANGED)              
*                                                                               
         MVC   KEY,RDARKEY                                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEYSAVE(RDARKRT-RDARKEY),KEY                                     
         BE    DELE0020                                                         
*                                                                               
         TM    DARFLAGS,DFXML      XML ORDER?                                   
         BZ    DELE0005            CHECK IF CONFIRMED ORDER EXISTS              
*                                                                               
         MVC   KEY,RDARKEY                                                      
         MVI   KEY,X'51'                                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEYSAVE(RDARKRT-RDARKEY),KEY                                     
         BNE   DELE0005                                                         
         MVI   EMAILFLG,EMFDUP     SEND DUPLICATE ERROR                         
         MVC   ERRCODE,=H'306'     DUPLICATE XML ORDER                          
         B     DELENO                                                           
*                                                                               
* EXACT KEY NOT FOUND                                                           
* NOW LOOK FOR THE ORDER # OF ALL OF THE OFFICES UNDER THIS AGENCY              
* (AGENCY OFFICE HAS CHANGED OR THIS IS A NEW ORDER)                            
*                                                                               
DELE0005 EQU   *                   LOOP THRU ALL OFFICES FOR THIS ORD#          
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKAOF-RDARKEY),RDARKEY                                    
*                                                                               
DELE0010 EQU   *                   LOOP THRU ALL OFFICES FOR THIS ORD#          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(RDARKAOF-RDARKEY),KEY                                    
         BNE   DELENO              NO  - SET CC TO NO                           
*                                                                               
DKEYD    USING RDARKEY,KEY                                                      
         MVC   DKEYD.RDARKORD,RDARKORD                                          
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                                                               
         CLC   KEYSAVE(RDARKRT-RDARKEY),KEY                                     
         BNE   DELE0015            SAME THRU ORDER NUMBER?                      
         OI    DARFLAGS,DFOFFCHG   SET OFFICE CHANGED FLAG                      
         B     DELE0020            SAME THRU ORDER NUMBER?                      
*                                                                               
DELE0015 DS    0H                                                               
         XC    KEY,KEY             SKIP TO NEXT OFFICE                          
         MVC   KEY(RDARKORD-RDARKEY),KEYSAVE                                    
         XR    R1,R1                                                            
         ICM   R1,3,DKEYD.RDARKAOF                                              
         AHI   R1,1                                                             
         STCM  R1,3,DKEYD.RDARKAOF                                              
         B     DELE0010                                                         
*                                                                               
* ARCHIVE EXISTING COPY TO X'4101' BEFORE DELETING THE RECORDS                  
*                                                                               
DELE0020 EQU   *                                                                
         MVC   ORDARKEY,KEY        SAVE OFF DARE KEY FOR RESTART                
*                                                                               
*        TM    DARFLAGS,DFXML      XML ORDER?                                   
*        BZ    DELE0025            ORDER EXISTS ALREADY IN REPPAK               
*        MVI   EMAILFLG,EMFDUP     SEND DUPLICATE ERROR                         
*        B     DELENO                                                           
*                                                                               
DELE0025 EQU   *                                                                
         GOTOR SAVECOPY                                                         
         B     DELE0050                                                         
*                                                                               
DELE0030 EQU   *                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRSEQ),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
DELE0040 EQU   *                                                                
         CLC   KEYSAVE(24),KEY     SAME KEY THROUGH ORDER NUMBER?               
         BE    DELE0050                                                         
         CLI   RESENTFL,C'Y'       NO, WAS ORDER FOUND?                         
         BE    DELEYES             SET FLAG AND EXIT                            
         B     DELENO                                                           
*                                                                               
*   BASIS FOR 'RESENT' FLAG IS THAT AGENCY ORDER HAD PREVIOUSLY BEEN            
*      RECEIVED ON THE REP SIDE.  THE FOLLOWING FOUR LINES OF CODE,             
*      COMMENTED OUT, CHANGE THE BASIS TO:  PREVIOUSLY RECEIVED, AND            
*      LINKED, WHERE IF UNLINKED = NEW RATHER THAN RESENT.                      
*                                                                               
DELE0050 EQU   *                                                                
***>     CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
***>     BNE   DELE0060            NO                                           
***>     OC    RDARREP#,RDARREP#   YES - LINKED?                                
***>     BZ    DELE0060            NO  -                                        
         MVI   RESENTFL,C'Y'       YES - SET 'ORDER RESENT=YES'                 
DELE0060 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                  YES - RETRIEVE RECORD                        
*                                                                               
*   RECORD DELETE BIT NO LONGER BEING SET                                       
*   UPDATED TO DELETE IF AGENCY OFFICE CHANGED (8/31/01 SKU)                    
*                                                                               
         TM    DARFLAGS,DFOFFCHG                                                
         BZ    *+8                                                              
         OI    RDARCNTL,X'80'      SET DELETE BIT                               
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   DELE0080            NO                                           
         MVC   SAVECON#,RDARREP#   SAVE OFF CONTRACT NUMBER                     
         MVC   SAVEMISC,RDARMISC   SAVE MISCELLANEOUS FLAG BYTE                 
*                                     OF ORIGINAL RECORD                        
         MVC   PREVNUM,RDARRNUM    SAVE PREVIOUS REVISION NUMBER                
         GOTOR SVOLDST             SAVE OLD DARE ORDER STATUS                   
DELE0080 EQU   *                                                                
*                                  SET SOFT/HARD DELETE FLAGS                   
         GOTOR CHKDAREC                                                         
         GOTO1 VDATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                  REWRITE DELETED REC                          
*                                                                               
*   KEY DELETE BIT NO LONGER BEING SET, KEY NOT REWRITTEN                       
*   UPDATED TO DELETE IF AGENCY OFFICE CHANGED (8/31/01 SKU)                    
*                                                                               
         TM    DARFLAGS,DFOFFCHG                                                
         BZ    DELE0030                                                         
         OI    KEY+27,X'80'        SET DELETE BIT IN KEY                        
         GOTO1 VDATAMGR,DMCB,(X'80',DMWRITE),=C'REPDIR',KEYSAVE,KEY             
*                                  REWRITE DELETED KEY                          
         B     DELE0030            GO BACK FOR NEXT                             
*                                                                               
DELEYES  SR    RC,RC                                                            
DELENO   LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
BUYDTL00 NTR1  BASE=*,LABEL=*      MOVE FOR ADDRESSSIBILITY                     
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PBUYDTLD,R4                                                      
*                                                                               
         NI    DARFLAGS,X'FF'-DFCEDATE  CLEAR CONFLICT END DATE FLAG            
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         CLC   INPROG,LASTREC      LAST RECORD WRITTEN OUT?                     
         BE    BUYD0040            YES                                          
         MVC   LASTREC,6(R3)       NO  - SET LAST RECORD TYPE                   
         BAS   RE,RECWRITE         WRITE OUT LAST RECORD                        
         MVI   RDARKSRT,X'30'      SET SUBRECORD TYPE TO 'BUY DETAIL'           
         XCEFL IOAREA+27,3973                                                   
*                                  CLEAR IOAREA: LEAVE KEY SET UP               
         LA    RF,38               SET INITIAL RECORD LENGTH                    
         STCM  RF,3,RDARLEN        INSERT INTO RECORD                           
         LA    R5,RDARELEM                                                      
         USING RDARBDEL,R5                                                      
         MVC   RDARBDCD(2),=X'0104'                                             
*                                  SET ELEMENT CODE/LENGTH                      
         CLI   DAILYFLG,C'D'       DAILY BUYLINE?                               
         BNE   BUYD0040            NO                                           
         OI    RDARBDFL,X'80'      YES - SET FLAG TO 'DAILY'                    
         DROP  R5                                                               
BUYD0040 EQU   *                                                                
         XC    ELTAREA(100),ELTAREA                                             
*                                  CLEAR WORKSPACE                              
         LA    R5,ELTAREA                                                       
         USING RDARBUEL,R5                                                      
         MVI   RDARBUCD,2          INSERT ELEMENT CODE                          
         LA    RF,RDARBULE                                                      
         STC   RF,RDARBULN         INSERT ELEMENT LENGTH                        
         PACK  DUB(8),PBDTCOST(9)                                               
*                                  CONVERT AND STORE COST, WITH CENTS           
         CVB   RF,DUB                                                           
         STCM  RF,15,RDARBU$$      INSERT COST                                  
         GOTO1 VDATCON,DMCB,(0,PBDTSTDT),(2,RDARBUSD)                           
*                                  INSERT START DATE                            
         PACK  DUB(8),PBDTNOWK(2)                                               
*                                  CONVERT AND STORE NUMBER PER WEEK            
         CVB   RF,DUB                                                           
         STCM  RF,1,BYTE           NUMBER OF WEEKS                              
*                                                                               
* CHECK TO SEE IF CONFLICT END DATE                                             
*                                                                               
         CLC   PBDTSPTS,FOXZEROS   SKIP ZERO SPOT BUYS                          
         BE    BUYD0060                                                         
*        GOTO1 VDATCON,DMCB,(2,SVESEND),(0,WORK)                                
*        CLC   PBDTSTDT,WORK       BUY START DATE AFTER EST END DATE            
*        BNL   BUYD0060            SKIP                                         
         CLI   DAILYFLG,C'D'       DAILY BUY?                                   
         BE    BUYD0060            YES,SHOULDN'T HAVE CONFLICT END DATE         
*                                                                               
         XC    WORK,WORK                                                        
         MHI   RF,7                CALCULATE END DATE                           
         GOTO1 VADDAY,DMCB,PBDTSTDT,WORK,(RF)                                   
         GOTO1 VGETDAY,DMCB,WORK,FULL                                           
         ZIC   RF,DMCB             START DATE + (# OF WEEKS X 7)                
*                                                                               
         LA    RE,7                RE=END DAY IN NUMERIC                        
         LA    R1,SVBYRO+L'RDARBYRO-1  SAVED BUY ROTATION                       
         CLI   0(R1),X'40'                                                      
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*&&DO                                                                           
         CLC   =C'40350003',PBDTORDR                                            
         BNE   TEST100                                                          
         CLC   =C'040301',PBDTSTDT                                              
         BNE   TEST100                                                          
         DC    X'0770'                                                          
*&&                                                                             
TEST100  DS    0H                                                               
         SR    RE,RF               END DAY MINUS START DAY                      
         BZ    BUYD0045            =0,SAME START, END DAY                       
         BP    BUYD0043            >0,NORMAL ROTATION                           
*                                                                               
         LR    RF,RE               <0,OUT OF WEEK ROTATIONS                     
         B     BUYD0050                                                         
*                                                                               
BUYD0043 DS    0H                  NORMAL ROTATION                              
         LA    RF,7                SUBTRACT BY 7                                
         LNR   RF,RF                                                            
         AR    RF,RE                                                            
         B     BUYD0050                                                         
*                                                                               
BUYD0045 DS    0H                  SAME START END DAY, BACK UP A WEEK           
         LA    RF,7                                                             
         LNR   RF,RF                                                            
*                                                                               
BUYD0050 DS    0H                  COMPUTE REAL END DATE FOR THIS DETL          
         GOTO1 VADDAY,DMCB,WORK,WORK,(RF)                                       
*                                  CONVERT ESTIMATE END DATE                    
         GOTO1 VDATCON,DMCB,(2,SVESEND),(0,WORK+6)                              
*&&DO                                                                           
         CLC   =C'40350003',PBDTORDR                                            
         BNE   TEST100                                                          
         CLC   =C'040301',PBDTSTDT                                              
         BNE   TEST100                                                          
         DC    X'0770'                                                          
TEST100  DS    0H                                                               
*&&                                                                             
         ZIC   RF,BYTE             RESTORE NUMBER/WK                            
         CLC   WORK(6),WORK+6      WORK:BUY END DATE                            
*                                  WORK+6:EST END DATE                          
         BNH   BUYD0060            NO CONFLICT                                  
*                                                                               
         OI    DARFLAGS,DFCEDATE   SET CONFLICT END DATE FLAG                   
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNZ   BUYD0060            IS THIS THE ONLY WEEK?                       
*                                                                               
         MVI   RDARBUWK,0          FAKE OUT ADDAY                               
         B     BUYD0065            YES, ONLY ADD EXPANDED 02 ELEMENT            
*                                                                               
BUYD0060 DS    0H                                                               
         STC   RF,RDARBUWK         INSERT NUMBER PER WEEK                       
         PACK  DUB(8),PBDTSPTS(2)                                               
*                                  CONVERT AND STORE SPOTS PER WEEK             
         CVB   RF,DUB                                                           
         STCM  RF,1,RDARBUSW       INSERT SPOTS PER WEEK                        
*                                                                               
         TM    DARFLAGS,DFXML      IF XML, SKIP 0 SPOT WEEKS                    
         BZ    BUYD0063                                                         
*                                                                               
         CLI   RDARBUSW,0                                                       
         BNE   BUYD0063                                                         
*                                                                               
         CLI   PBDTCONT,C' '       LAST BUY DETAIL?                             
         BH    BUYDX                                                            
*                                                                               
         LA    R6,IOAREA           YES, MUST HAVE AT LEAST ONE                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    BUYDX                                                            
*                                                                               
BUYD0063 DS    0H                                                               
         BAS   RE,GRSTOTLS         ESTABLISH GROSS TOTALS                       
         BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
         TM    DARFLAGS,DFCEDATE                                                
         BZ    BUYDX                                                            
*                                                                               
BUYD0065 DS    0H                                                               
         LA    RF,RDARELEM         YES - SET FLAG TO CONFLICT END DATE          
         OI    RDARBDFL-RDARBDEL(RF),X'10'                                      
*                                                                               
         MVI   RDARBULN,RDARBUL2   BUILD AN EXPANDED 02 ELEMENT                 
*                                                                               
         PACK  DUB(8),PBDTSPTS(2)                                               
*                                  CONVERT AND STORE SPOTS PER WEEK             
         CVB   RF,DUB                                                           
         STCM  RF,1,RDARBUSW       INSERT SPOTS PER WEEK                        
*                                                                               
         ZIC   RF,RDARBUWK         CALCULATE START DATE                         
         MHI   RF,7                                                             
         GOTO1 VADDAY,DMCB,PBDTSTDT,WORK,(RF)                                   
         GOTO1 VDATCON,DMCB,(0,WORK),(2,RDARBUSD)                               
         MVI   RDARBUWK,1          ONE WEEK                                     
         GOTO1 VDATCON,DMCB,(2,SVESEND),(0,WORK)                                
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
         ZIC   RF,DMCB                                                          
         LA    RE,7                                                             
         MVC   WORK(L'SVBYRO),SVBYRO                                            
         LA    R1,WORK+L'RDARBYRO-1                                             
BUYD0070 DS    0H                                                               
         CR    RE,RF                                                            
         BE    BUYD0080                                                         
         MVI   0(R1),C' '          MODIFY ROTATION                              
         BCTR  RE,0                                                             
         BCT   R1,BUYD0070                                                      
BUYD0080 DS    0H                                                               
         MVC   RDARBURO(L'SVBYRO),WORK    INSERT MODIFIED ROTATION              
         BAS   RE,GRSTOTLS         ESTABLISH GROSS TOTALS                       
         BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
BUYDX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
GRSTOTLS NTR1                                                                   
         SR    RE,RE                                                            
         ZIC   RF,RDARBUSW         LOAD # SPOTS/WEEK                            
         ZIC   R1,RDARBUWK         LOAD # WEEKS                                 
         MR    RE,R1               SPOTS/WEEK * WEEKS = TOTAL SPOTS             
         L     RE,GROSPOTS         LOAD GROSS SPOTS                             
         AR    RE,RF               ADD TOTAL SPOTS TO GROSS                     
         ST    RE,GROSPOTS         SAVE GROSS SPOTS                             
         SR    RE,RE               CLEAR                                        
         ZICM  R1,RDARBU$$,4       LOAD SPOT COST                               
         MR    RE,R1               TOTAL SPOTS * COST = TOTAL $$                
         L     RE,GROSDOLS         LOAD GROSS DOLLARS                           
         AR    RE,RF               ADD TOTAL $$ TO GROSS $$                     
         ST    RE,GROSDOLS         SAVE GROSS DOLLARS                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
* DUMP KEY BUFF TO X'55' RECORD FOR DEBUGGING                                   
*                                                                               
**********************************************************************          
DUMPBUFF NTR1  BASE=*,WORK=(R4,DBWORKQ),LABEL=*                                 
         USING DBWORKD,R4                                                       
*                                                                               
*        LA    R6,IOAREA           SET OUTPUT AREA DSECT                        
*        USING RDARREC,R6                                                       
*        CLC   =X'30340002',RDARKORD                                            
*        BNE   DSX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'55'           SCREEN DUMP RECORD                           
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DS100                                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               DBIO,DBWORK                                                      
*                                                                               
* DELETED ALL X'20' ELEMENTS                                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'20',DBIO),0,0                  
         B     DS110                                                            
*                                                                               
DS100    DS    0H                                                               
         XCEFL DBIO,3650                                                        
*                                                                               
         LA    R6,DBIO                                                          
         USING RSCNREC,R6                                                       
         MVI   RSCNKTYP,RSCNTYPQ                                                
         MVC   RSCNLEN,=H'35'                                                   
         DROP  R6                                                               
*                                                                               
* BUG TRIGGERED, RECORD ORDER KEY                                               
*                                                                               
DS110    DS    0H                                                               
         TM    DARFLAGS,DFBUG                                                   
         BZ    DS113                                                            
*                                                                               
* DELETED ALL X'1F' ELEMENTS                                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'1F',DBIO),0,0                  
*                                                                               
         NI    DARFLAGS,X'FF'-DFBUG                                             
         XC    DBELEM,DBELEM                                                    
         LA    R6,DBELEM                                                        
         USING RSCNDKYE,R6                                                      
         MVI   RSCNDKYE,X'1F'                                                   
         MVI   RSCNDKYN,RSCNDKYQ                                                
         L     R3,APASAREA                                                      
         MVC   RSCNDKEY,800(R3)    R3->NEW PASSIVE POINTERS                     
         DROP  R6                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),DBIO,DBELEM,=C'ADD=END'           
         CLI   DMCB+12,0                                                        
         BNE   DS120                                                            
*                                                                               
DS113    DS    0H                                                               
         L     R3,APASAREA                                                      
         XR    R2,R2                                                            
*                                                                               
DS115    DS    0H                                                               
         XC    DBELEM,DBELEM                                                    
         LA    R6,DBELEM                                                        
         USING RSCNDMPE,R6                                                      
         MVI   RSCNDMPE,X'20'                                                   
         MVI   RSCNDMLN,X'FF'                                                   
         MVC   RSCNDMP(253),0(R3)                                               
         DROP  R6                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),DBIO,DBELEM,=C'ADD=END'           
         CLI   DMCB+12,0                                                        
         BNE   DS120                                                            
*                                                                               
         XC    DBELEM,DBELEM                                                    
         LA    R6,DBELEM                                                        
         USING RSCNDMPE,R6                                                      
         MVI   RSCNDMPE,X'20'                                                   
         MVI   RSCNDMLN,X'FF'                                                   
         MVC   RSCNDMP(253),253(R3)                                             
         DROP  R6                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),DBIO,DBELEM,=C'ADD=END'           
         CLI   DMCB+12,0                                                        
         BNE   DS120                                                            
*                                                                               
         LTR   R2,R2                                                            
         BNZ   DS120                                                            
*                                                                               
         L     R3,APASAREA                                                      
         LA    R3,800(R3)          R3->NEW PASSIVE POINTERS                     
         LA    R2,1                                                             
         B     DS115                                                            
*                                                                               
DS120    DS    0H                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DS130                                                            
         GOTO1 VDATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,        X        
               DBIO,DBWORK                                                      
         B     DSX                                                              
*                                                                               
DS130    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),DBIO                                                     
         GOTO1 VDATAMGR,DMCB,(X'88',ADDREC),=C'REPFILE',KEY,           X        
               DBIO,DBWORK                                                      
*                                                                               
DSX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
* ARCHIVE EXISTING COPY TO X'4101' BEFORE DELETING THE RECORDS                  
*                                                                               
**********************************************************************          
SAVECOPY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),ORDARKEY                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   SAVX                                                             
*                                                                               
* DELETE ALL EXISTING X'4101' SHADOW RECORDS FIRST                              
*                                                                               
         MVI   KEY+1,X'01'                                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
SAVCP10  DS    0H                                                               
         CLC   KEYSAVE(RDARKRT-RDARKEY),KEY                                     
         BNE   SAVCP50             YES                                          
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
         OI    IOAREA+29,X'80'                                                  
*                                  MARK RECORD DELETED                          
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
         OI    KEY+27,X'80'        MARK KEY DELETED                             
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEYSAVE,KEY             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRSEQ),=C'REPDIR',KEYSAVE,KEY                  
         B     SAVCP10                                                          
*                                                                               
SAVCP50  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),ORDARKEY                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   SAVX                                                             
*                                                                               
SAVCP60  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,            X        
               IOAREA,IOWORK                                                    
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2                                                       
         CLI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
         BNE   SAVCP80                                                          
         L     RF,APASAREA         CLEAR FIRST POSITIONS OF WORK AREA           
         XC    0(128,RF),0(RF)                                                  
*                                                                               
*** TESTING                                                                     
*        CLC   ERRORD#,=CL8'20000000'                                           
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*** TESTING                                                                     
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',VCOMFACS),APASAREA,(R2),X        
               ASPLAREA                                                         
*                                                                               
*                                                                               
SAVCP80  DS    0H                                                               
         MVC   SVCPYKEY,KEY                                                     
*                                                                               
         MVI   KEY+1,X'01'                                                      
         MVI   IOAREA+1,X'01'                                                   
         BAS   RE,RECWRITE                                                      
*                                                                               
         MVC   KEY,SVCPYKEY                                                     
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   SAVX                                                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRSEQ),=C'REPDIR',KEYSAVE,KEY                  
         CLC   KEYSAVE(RDARKRT-RDARKEY),KEY                                     
         BE    SAVCP60                                                          
*                                                                               
SAVX     DS    0H                  RESTORE KEY BEFORE EXIT                      
         XC    KEY,KEY                                                          
         MVC   KEY(27),ORDARKEY                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* IF DARE REVISION, RESET REVISION NUMBER IN CONTRACT                           
* ALSO RESET RECALLED FLAG                                                      
**********************************************************************          
RESETREV NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYCAND,R4                                                      
*                                                                               
         XC    KEY,KEY             CLEAR CONTRACT KEY AREA                      
KYD      USING RCONKEY,KEY                                                      
         MVI   KYD.RCONPTYP,X'8C'      SET KEY TYPE 8C                          
         MVC   KYD.RCONPREP,POWERCDE   INSERT REP ID                            
*                                  COMPLEMENT CONTRACT NUMBER                   
         ZAP   WORK+15(5),=P'99999999'                                          
         GOTO1 VHEXIN,DMCB,PACNRPCN,WORK+20,8,0                                 
         MVO   WORK+15(5),WORK+20(4)                                            
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KYD.RCONPCON+0(4),WORK+15                                        
         DROP  KYD                                                              
*                                  INSERT CONTRACT NUMBER, 9'S COMP             
*                                     USED AGAIN FOR REVERSED.                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BNE   RESETVX             YES -                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,        X        
               ACONAREA,IOWORK                                                  
*                                                                               
         L     R2,ACONAREA                                                      
         LA    R2,34(R2)                                                        
*                                                                               
RESETV10 DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    RESETVX                                                          
         CLI   0(R2),X'1D'                                                      
         BE    RESETV20                                                         
         BH    RESETVX                                                          
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     RESETV10                                                         
*                                                                               
RESETV20 DS    0H                                                               
         USING RCONDREL,R2                                                      
* RESET RECALL FLAG                                                             
         NI    RCONDRFG,X'FF'-X'10'                                             
*                                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    RESETVX                                                          
*                                                                               
         MVI   RCONDRRV,0          RESET REVISION NUMBER                        
*                                                                               
         OI    RCONDRF2,X'02'      REVISION CANCELLED                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               ACONAREA,IOWORK                                                  
*                                                                               
RESETVX  DS    0H                                                               
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*    COMPARE: COMPARE THE INCOMING DARE ORDER WITH CONTRACT AND CHANEG          
*             STATUS BASE ON THE ORIGINAL DARE STATUS                           
*    R3-> AIO2    -> CON REC                                                    
*    R2-> IOAREA  -> DAR REC                                                    
***********************************************************************         
COMPARE  NTR1  BASE=*,LABEL=*                                                   
         OC    QREPCON,QREPCON                                                  
         BZ    COMPARY                                                          
         CLI   OLDBSTS,C'R'        REJECT                                       
         BE    COMPARY             NO COMPARE FOR REJECT                        
         CLI   OLDBSTS,C'C'        REJECT                                       
         BE    COMPARY             NO COMPARE FOR RECALL                        
         TM    OLDFLG1,X'20'       IF PENDCF, SKIP THIS CHECK                   
         BO    *+12                                                             
         TM    DARFLAGS,DFCONCF    CONTRACT IS CONFIRMED NOW?                   
         BO    COMPARY             YES, NO COMPARISON IS DONE                   
         CLI   OLDBSTS,C'M'        AMEND-> RESEND                               
         BNE   *+12                REGARDLESS OF MATCH OR NOT                   
         TM    OLDFLG1,X'40'       SO DO NOT NEED TO COMPARE                    
         BZ    COMPARY                                                          
*                                                                               
* READ DARE RECORD INTO IOAREA                                                  
         XCEFL IOAREA,4000                                                      
         L     RE,AIO2                                                          
         LA    RF,4000                                                          
         XCEFL                                                                  
*                                                                               
* READ CONTRACT RECORD INTO AIO2                                                
         ZAP   WORK(8),=P'99999999'                                             
         PACK  DUB,QREPCON                                                      
         SP    WORK(8),DUB(8)                                                   
         L     R0,WORK                                                          
         L     R1,WORK+4                                                        
         SRDL  R0,4                                                             
         ST    R1,FULL             9'S COMPLEMENT CONTRACT NUMBER               
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCONKEY,KEY                                                      
         MVI   K.RCONPTYP,X'8C'      CONTRACT PASSIVE PTR 1                     
         MVC   K.RCONPREP,POWERCDE                                              
         MVC   K.RCONPCON,FULL                                                  
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   COMPARY                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,            X        
               AIO2,IOWORK                                                      
         L     R3,AIO2                                                          
         USING RCONREC,R3                                                       
*                                                                               
* READ DARE RECORD INTO IOAREA                                                  
*                                                                               
         MVC   KEY,SVDARKEY                                                     
K        USING RDARKEY,KEY                                                      
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BNE   COMPARY             NO SUCH RECORD, GET OUT                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          FIRST BUILD KEY                              
*                                                                               
         CLI   RDARRNUM,0          FOR REVNEW,                                  
         BE    CP005                                                            
         CLI   RDARBSTS,0          SKIP COMPARISON                              
         BNE   CP005                                                            
         CLI   RDARMISC,0                                                       
         BE    COMPARY                                                          
*                                                                               
CP005    DS    0H                                                               
         LA    RF,RDARELEM         YES - SET A(01 ELEMENT)                      
CP010    EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    CP018               NO X'0F' ELT: BUILD ONE                      
         CLI   0(RF),X'0F'         MISC FLAG ELT?                               
         BE    CP015               YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     CP010               GO BACK FOR NEXT                             
*                                                                               
CP018    EQU   *                                                                
         XC    ELTAREA,ELTAREA                                                  
         MVC   ELTAREA(2),=X'0F0A'    BUILD A 0F ELT                            
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA                    
         B     CP005                                                            
*                                                                               
CP015    DS    0H                                                               
         LR    R4,RF                                                            
         USING RDARFLEM,R4         R4->RDARFLEM                                 
* TEST TO SEE IF PENDING CONFIRM                                                
*                                                                               
         TM    OLDFLG1,X'20'                                                    
         BZ    CP050               PENDING?                                     
*                                                                               
         LA    RF,RCONELEM         YES - SET A(01 ELEMENT)                      
CP020    EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    CP050               NO X'1F', PROCEED                            
         CLI   0(RF),X'20'         MISC FLAG ELT?                               
         BE    CP028               YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     CP020               GO BACK FOR NEXT                             
*                                                                               
CP028    EQU   *                                                                
         USING RCONSEND,RF                                                      
         TM    RCONSENF,X'10'+X'20' CONTRACT IS IN WIP?                         
         BNO   CPNO                YES, THEN SET TO RESEND                      
*                                  WITHOUT COMPARING                            
         DROP  RF                                                               
*                                                                               
CP050    DS    0H                                                               
         MVC   WORK(4),VCOMFACS                                                 
         L     RE,=V(REDARTKO)                                                  
         A     RE,RELO                                                          
         STCM  RE,15,WORK+4                                                     
         L     RE,=V(GETBROAD)                                                  
         A     RE,RELO                                                          
         STCM  RE,15,WORK+8                                                     
         MVC   KEYSAVE,SVDARKEY                                                 
K        USING RDARKEY,KEYSAVE                                                  
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         GOTO1 =V(REREPDIF),DMCB,(X'80',KEYSAVE),WORK,RR=RELO                   
         BNE   CPNO                CONTRACT <> ORDER                            
*                                                                               
CPYES    DS    0H                  CONTRACT = ORDER                             
         TM    OLDFLG1,X'20'       PENDING CF?                                  
         BZ    CPY010                                                           
         GOTOR CLEANUP             YES, CONFIRM CONTRACT DIRECTLY               
         MVI   AUDITYP,DHCONFIQ                                                 
         GOTOR DOAUDIT                                                          
         B     COMPARN             SET CONDITION CODE                           
*                                                                               
CPY010   DS    0H                                                               
         TM    OLDFLG1,X'10'       STACF                                        
         BZ    CPY020                                                           
         GOTOR SCFDT               SAVE STACF DATE/TIME                         
         OI    RDARFLG1,X'10'      STAY AS STACF                                
         MVI   AUDITYP,DHSTACFQ                                                 
         B     CP100                                                            
*                                                                               
CPY020   DS    0H                                                               
         CLI   OLDBSTS,C'M'        AMEND?                                       
         BNE   CP100                                                            
         TM    OLDFLG1,X'40'       -S?                                          
         BZ    CPNO                AMEND -> RESEND                              
*                                                                               
         OI    RDARFLG1,X'C0'      AMEND-S -> MATCH-S                           
         MVI   RDARBSTS,C'M'       KEEP AMEND STATUS FOR PASSIVE KEYS           
         MVI   AUDITYP,DHMATCHQ                                                 
         B     CP100                                                            
                                                                                
*                                                                               
CPNO     DS    0H                  CONTRACT <> ORDER                            
         XC    RDARBSTS,RDARBSTS                                                
         OI    RDARMISC,X'80'      RESENT                                       
*                                                                               
CP100    DS    0H                                                               
         L     R4,APASAREA         UPDATE NEW PASSIVE POINTER                   
         LA    R4,800(R4)                                                       
         LA    R2,IOAREA                                                        
*                                  CLEAR FIRST POSITIONS OF WORK AREA           
         XC    0(128,R4),0(R4)                                                  
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',VCOMFACS),(R4),(R2),    X        
               ASPLAREA                                                         
*                                                                               
*                                                                               
         MVC   KEY,SVDARKEY                                                     
K        USING RDARKEY,KEY                                                      
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BNE   COMPARY             NO SUCH RECORD, GET OUT                      
*                                                                               
* NOW UPDATE RECORD AS WELL                                                     
         L     R4,ACONAREA                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R4),IOWORK                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
COMPARY  SR    RC,RC                                                            
COMPARN  LTR   RC,RC                                                            
COMPAREX DS    0H                                                               
         DROP  R2,R3,R4                                                         
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO ADD STACF DATE/TIME ELEMENT TO DARE RECORD                         
**********************************************************************          
SCFDT    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'50',IOAREA),0,0                
         XC    ELTAREA,ELTAREA                                                  
K        USING RDARSTEM,ELTAREA                                                 
         MVI   ELTAREA,X'50'                                                    
         MVI   K.RDARSTLN,RDARSTLQ                                              
         GOTO1 VDATCON,DMCB,(5,0),(2,K.RDARSTDT)                                
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,K.RDARSTTM                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA                    
         DROP  K                                                                
SCFDTX   EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO DELETE ALL PASSIVE POITERS TO 51 RECORDS                           
**********************************************************************          
DELPAS   NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY,SVDARKEY                                                     
K        USING RDARKEY,KEY                                                      
         MVI   KEY,X'51'                                                        
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BNE   DELPASX             YES -                                        
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
         LA    R2,IOAREA                                                        
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        APASAREA: KEY BUILD AREA                                               
*        R2  : CURRENT LOCATION OF AGENCY ORDER RECORD                          
*        ASPLAREA: IO AREA                                                      
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'81',VCOMFACS),APASAREA,(R2),X        
               ASPLAREA                                                         
*                                                                               
*                                                                               
*   DELETE ALL 51 PASSIVES                                                      
*        APASAREA: KEY BUILD AREA                                               
*        R2  : CURRENT LOCATION OF AGENCY ORDER RECORD                          
*        ASPLAREA: IO AREA                                                      
*                                                                               
         L     R4,APASAREA         A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->ALL NULL                                 
         LHI   RF,800              THIS WILL DELETE ALL THE OLD PTS             
         XCEF  (R4)                                                             
*                                                                               
         L     R3,APASAREA                                                      
DELP003  EQU   *                                                                
         CLI   0(R3),0             END OF KYES                                  
         BE    DELP020                                                          
         CLC   =X'D102',0(R3)      LOOKING FOR D102, D001, D002                 
         BE    DELP005                                                          
         CLC   =X'D001',0(R3)                                                   
         BE    DELP005                                                          
         CLC   =X'D002',0(R3)                                                   
         BNE   DELP010             NEXT PASSIVE KEY                             
*                                                                               
DELP005  EQU   *                                                                
         MVC   KEY(27),0(R3)                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
         LA    R6,IOAREA                                                        
         CLI   0(R6),X'41'         IF THIS PASSIVE KEY->41 RECORDS              
         BNE   DELP010                                                          
         MVC   800(32,R3),0(R3)    THEN COPY THE KEY OVER SO                    
DELP010  EQU   *                   THE PASSIVE KEY WON'T BE DELETED             
         LA    R3,32(R3)                                                        
         B     DELP003                                                          
*                                                                               
DELP020  DS    0H                                                               
*** TESTING                                                                     
         CLC   ERRORD#,=CL8'20000000'                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*** TESTING                                                                     
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',VCOMFACS),APASAREA,(R4),X        
               HDRDA                                                            
*                                                                               
         MVC   KEY,SVDARKEY                                                     
K        USING RDARKEY,KEY                                                      
         MVI   KEY,X'51'                                                        
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BNE   DELPASX             YES -                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DELPASX                                                          
         USING RDARFLEM,R6                                                      
         OI    RDARFLG1,X'02'      TYPE 41 RECORD EXIST FOR THIS ORDER          
* NOW UPDATE RECORD                                                             
         GOTO1 VDATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
         DROP  R6                                                               
DELPASX  B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  SVOLDST:  SAVE OLD DARE ORDER STATUS                                         
*    IOAREA  ->DAR REC                                                          
***********************************************************************         
SVOLDST  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
*                                                                               
         MVC   OLDBSTS,RDARBSTS                                                 
*                                                                               
         LA    RF,RDARELEM         YES - SET A(01 ELEMENT)                      
SV005    EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    SV010               NO X'0F' ELT: EXIT                           
         CLI   0(RF),X'0F'         MISC FLAG ELT?                               
         BE    SV008               YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     SV005               GO BACK FOR NEXT                             
*                                                                               
SV008    EQU   *                                                                
         USING RDARFLEM,RF                                                      
         MVC   OLDFLG1,RDARFLG1                                                 
*                                                                               
SV010    EQU   *                                                                
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'0A'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SVOLDSTX                                                         
         USING RDARPPEL,R6                                                      
         MVC   SVSPCODE,RDARPPSP                                                
SVOLDSTX DS    0H                                                               
         DROP  R6,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  CLEANUP:  CONFIRM CONTRACT (MATCHING & PENDCF)                               
*    IOAREA  ->DAR REC                                                          
*    AIO2    ->CON REC                                                          
***********************************************************************         
CLEANUP  NTR1  BASE=*,LABEL=*                                                   
         L     RE,APASAREA         CLEAR PASSIVE KEY BUILD AREA                 
         LA    RF,L'PASAREA                                                     
         XCEFL                                                                  
         XC    HDRDA,HDRDA                                                      
*                                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
*                                                                               
K        USING RDARKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'RDARKEY),SVDARKEY     DELETE ALL OLD 51 RECORDS            
         MVI   KEY,X'51'                                                        
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                                                               
CLEAN02  DS    0H                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE                                     
         BNE   CLEAN03             DONE                                         
*        MVI   UPDATE,C'Y'                                                      
*        GOTO1 VGETREC,DMCB,(R6)                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R6),IOWORK                                                      
*                                                                               
         CLI   KEY+RDARKRT-RDARKEY,X'10'                                        
         BNE   CLEAN02A                                                         
         GOTOR DELPAS              DELETE PASSIVE PTS                           
*                                                                               
CLEAN02A DS    0H                                                               
         OI    RDARCNTL,X'80'                                                   
         GOTO1 VDATAMGR,DMCB,PUTREC,=C'REPFILE',KEY+28,                X        
               (R6),IOWORK                                                      
*                                                                               
         OI    KEY+27,X'80'                                                     
*        MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,DMWRITE,=C'REPDIR',KEYSAVE,KEY                     
         GOTO1 VDATAMGR,DMCB,(X'80',DMRSEQ),=C'REPDIR',KEYSAVE,KEY              
*                                                                               
         B     CLEAN02                                                          
*                                                                               
CLEAN03  DS    0H                                                               
K        USING RDARKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'RDARKEY),SVDARKEY     DELETE ALL OLD 51 RECORDS            
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ADD HEADER RECORD BACK AS X'51'                                               
*                                                                               
CLEAN30  DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R6),IOWORK                                                      
*                                                                               
         CLI   KEY+RDARKRT-RDARKEY,X'10'                                        
         BNE   CLEAN30A            ONLY FOR HEADER RECORD                       
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        APASAREA: KEY BUILD AREA                                               
*        IOAREA : CURRENT LOCATION OF AGENCY ORDER RECORD                       
*        ASPLAREA: IO AREA                                                      
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',VCOMFACS),APASAREA,(R6),X        
               ASPLAREA                                                         
*                                                                               
*                                                                               
CLEAN30A DS    0H                                                               
         OI    RDARCNTL,X'80'      DELETE 41 RECORD                             
         GOTO1 VDATAMGR,DMCB,PUTREC,=C'REPFILE',KEY+28,                X        
               (R6),IOWORK                                                      
         MVI   KEY+27,X'80'                                                     
         GOTO1 VDATAMGR,DMCB,DMWRITE,=C'REPDIR',KEYSAVE,KEY                     
*                                  DELETE 41 KEY                                
*                                                                               
CLEAN35  DS    0H                                                               
         GOTO1 =A(CKDAREC),RR=RELO  SOFT/HARD DELETES?                          
         BNZ   CLEAN60             YES - DON'T WRITE 51 REC                     
*                                                                               
         MVI   RDARKTYP,X'51'      REWRITE HEADER AS X'51'                      
         MVI   RDARCNTL,0                                                       
*                                                                               
         CLI   RDARKRT,X'10'       HEADER RECORD?                               
         BNE   CLEAN45                                                          
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
*                                                                               
         USING RDARFLEM,R6                                                      
         NI    RDARFLG1,X'FF'-X'02' DO GENERATE PASSIVES(OVNIGHT PROC)          
         DROP  R6                                                               
         LA    R6,IOAREA           RESET                                        
         USING RDARREC,R6                                                       
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*        APASAREA:   KEY BUILD AREA                                             
*        (R6)   : CURRENT LOCATION OF AGENCY ORDER RECORD                       
*        ASPLAREA:  IO AREA                                                     
*                                                                               
         L     R4,APASAREA         SET A(KEY BUILD AREA)                        
         A     R4,=F'800'          ADD 800 TO ADDRESS                           
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',VCOMFACS),(R4),(R6),    X        
               ASPLAREA                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
* ADD ACTIVITY ELEMENT                                                          
*                                                                               
CLEAN40  DS    0H                                                               
         XC    ELTAREA,ELTAREA                                                  
         LA    R5,ELTAREA                                                       
         USING RDARCFEM,R5                                                      
         MVI   RDARCFCD,X'F1'                                                   
         MVI   RDARCFLN,RDARCFLQ                                                
*                                                                               
* GET TODAY'S DATE AND TIME                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,RDARCFDT)                                  
         TIME  DEC                 R0 HAS HH:MM:SS:TT                           
         STCM  R0,4,RDARCFTM+1     SAVE MM                                      
         SRL   R0,24                                                            
         LR    R4,R0                                                            
         LA    R4,DDSTMADJ(R4)     ADD HH FOR DDS CLOCK                         
         EDIT  (R4),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 VHEXIN,DMCB,WORK+17,RDARCFTM,2,0                                 
         DROP  R5                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),(R6),ELTAREA,            X        
               =C'ADD=END'                                                      
*                                                                               
* ADDITIONAL CHECK FOR KATZ/EDI ORDERS FOR DUPLICATE X'51' RECORDS ON           
* FILE                                                                          
*                                                                               
CLEAN45  DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
         MVC   KEY(L'RDARKEY),RDARKEY                                           
*  READ DELETES, FOR UPDATE                                                     
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                                                               
         CLC   KEY(L'RDARKEY),KEYSAVE  '51' REC AREADY ON FILE?                 
         BE    CLEAN50                 YES - OVERWRITE IT                       
         GOTO1 VDATAMGR,DMCB,(X'88',ADDREC),=C'REPFILE',KEY,           X        
               (R6),IOWORK                                                      
         MVC   HDRDA,KEY           SAVE D/A TO UPDATE PASSIVE PTS               
         B     CLEAN60                                                          
         DROP  R6                                                               
*                                                                               
CLEAN50  DS    0H                  RESTORE ACTIVE KEY IF NEEDED                 
         TM    KEY+27,X'80'        KEY DELETED?                                 
         BZ    CLEAN55                                                          
         MVI   KEY+27,0                                                         
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEYSAVE,KEY             
*                                                                               
CLEAN55  DS    0H                  REWRITE OLD 51 RECORD                        
         MVC   HDRDA,KEY+28                                                     
         L     R3,ASPLAREA                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,        X        
               (R3),IOWORK                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               (R6),IOWORK                                                      
*        GOTO1 VGETREC,DMCB,ASPLAREA   USE SPOOL                                
*        GOTO1 VMOVEREC,DMCB,(R6),ASPLAREA                                      
*        GOTO1 VPUTREC,DMCB,ASPLAREA                                            
*                                                                               
* REREAD HEADER KEY AND MARK DELETED                                            
*                                                                               
CLEAN60  DS    0H                                                               
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         LA    R6,IOAREA                                                        
         USING RDARKEY,R6                                                       
         CLI   RDARKRT,X'10'       HEADER RECORD?                               
         BNE   CLEAN60A                                                         
         DROP  R6                                                               
*                                                                               
         L     R4,APASAREA         A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->NEW PASSIVE POINTERS                     
         OC    HDRDA,HDRDA                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',VCOMFACS),APASAREA,(R4),X        
               HDRDA                                                            
*                                                                               
CLEAN60A DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
*                                                                               
         MVC   KEY(L'RDARKEY),RDARKEY                                           
         MVI   KEY,X'41'                                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'08',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
                                                                                
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMRSEQ),=C'REPDIR',KEYSAVE,KEY              
*                                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE  SAME ORDER?                        
         BE    CLEAN30                       YES - PROCESS RECORD               
*                                                                               
* REMOVE X'0B01' SHADOW AGENCY BUY RECORDS, IF ANY                              
*                                                                               
         ZAP   WORK(8),=P'99999999'                                             
         PACK  DUB,QREPCON                                                      
         SP    WORK(8),DUB(8)                                                   
         L     R0,WORK                                                          
         L     R1,WORK+4                                                        
         SRDL  R0,4                                                             
         ST    R1,FULL             9'S COMPLEMENT CONTRACT NUMBER               
*                                                                               
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         XC    KEY,KEY                                                          
         MVC   RBUYKTYP(2),=X'0B01'                                             
         MVC   RBUYKREP,POWERCDE                                                
         MVC   RBUYKCON,FULL                                                    
         DROP  R6                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                                                               
CLEAN80  DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   CLEANUPX            DONE                                         
*                                                                               
         LA    R6,IOAREA                                                        
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R6),IOWORK                                                      
*                                                                               
         USING RBUYREC,R6                                                       
         OI    RBUYCNTL,X'80'                                                   
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               (R6),IOWORK                                                      
         DROP  R6                                                               
*                                                                               
         OI    KEY+27,X'80'                                                     
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEYSAVE,KEY             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',DMRSEQ),=C'REPDIR',KEYSAVE,KEY              
         B     CLEAN80                                                          
CLEANUPX DS    0H                                                               
         GOTOR SNDCFNOT                                                         
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CKDAREC :  FOR DIFFERENT TYPES OF DARE RECORD, CHECK IF DELETE                
*  BIT HAS BEEN TURNED ON. IF IT IS, SKIP REWRITING IT BACK AS X'51'            
**********************************************************************          
CKDAREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6          SET OUTPUT RECORD AREA                       
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   CKDR0040            NO                                           
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARELEM,R3                                                      
         TM    RDARDELS,X'80'+X'40'                                             
         B     CKDRXIT                                                          
         DROP  R3                                                               
*                                                                               
CKDR0040 EQU   *                                                                
         CLI   RDARKRT,X'40'       BUY?                                         
         BNE   CKDR0080            NO                                           
         CLI   RDARKSRT,X'00'      BUY HEADER?                                  
         BNE   CKDR0120            NO  - ORB/COMMT/DETAIL                       
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARBYEL,R3                                                      
         TM    RDARBYDL,X'80'+X'40'                                             
         B     CKDRXIT                                                          
         DROP  R3                                                               
*                                                                               
CKDR0080 EQU   *                                                                
         BH    CKDRNO              SKIP OVER RECORD TYPES                       
*                                     50 (TRAILER) + 60 (EQUIVS)                
*                                                                               
*   REMAINING RECORD TYPES ARE 15, 20, 30, 40/10, 40/20, 40/30, AND             
*        SOFT/HARD DELETE BYTE IS IN SAME PLACE IN ALL ELEMENTS.                
*        STANDARD COMMENT ELEMENT FORMAT USED, ARBITRARILY.                     
*                                                                               
CKDR0120 EQU   *                                                                
         LA    R3,RDARELEM                                                      
         USING RDARELE2,R3                                                      
         TM    RDARELDL,X'80'+X'40'                                             
         BNZ   CKDRYES                                                          
         DROP  R3                                                               
*                                                                               
CKDRNO   SR    RC,RC                                                            
CKDRYES  LTR   RC,RC                                                            
CKDRXIT  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD DARE CONFIRMATION AUDIT TRAIL TO X'51' RECORD                             
***********************************************************************         
DOAUDIT  NTR1  BASE=*,WORK(R4,500),LABEL=*                                      
*** TESTING                                                                     
         CLC   ERRORD#,=CL8'20000000'                                           
         BE    DOAUDX                                                           
*** TESTING                                                                     
         OC    AUDITYP,AUDITYP                                                  
         BZ    DOAUDX                                                           
*                                                                               
         XC    KEY,KEY                                                          
KEYD     USING RDARKEY,KEY                                                      
         MVC   KEY(RDARKRT-RDARKEY),SVDARKEY                                    
         MVI   KEY,X'41'                                                        
         CLI   AUDITYP,DHCONFIQ                                                 
         BNE   *+8                                                              
         MVI   KEY,X'51'                                                        
         MVI   KEYD.RDARKRT,X'70'  GET TRAILER RECORD                           
         DROP  KEYD                                                             
*                                                                               
*        GOTO1 VHIGH                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DOAUDX                                                           
*                                                                               
*        MVI   UPDATE,C'Y'                                                      
*        GOTO1 VGETREC,DMCB,(R4)                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R4),IOWORK                                                      
*                                                                               
         MVC   WORK(4),VHELLO       RECORD DARE HISTORY                         
         MVC   WORK+4(4),VDATCON                                                
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+4,X'FF'        VALID ACTION                                 
         MVC   DMCB+5(1),AUDITYP   ACTION                                       
         MVC   DMCB+6(1),REVNUM    REVISION NUMBER                              
         MVI   DMCB+7,HFGSRQ       THIS IS DONE IN SERVICE REQUEST              
         GOTO1 =V(REGENDHT),DMCB,(R4),,WORK,RR=RELO                             
*                                                                               
*        GOTO1 VPUTREC,DMCB,(R4)                                                
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               (R4),IOWORK                                                      
*                                                                               
DOAUDX   B     EXIT                                                             
         XC    AUDITYP,AUDITYP                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SENDS A CONFIRM NOTIFICATION OUT                                              
* IOAREA MUST HAVE DARE REC ALL THE WAY THROUGH                                 
* ACONAREA IS USED FOR VARIOUS PURPOSE IO READS                                 
* ASPLAREA IS USED FOR SPOOL PURPOSE                                            
***********************************************************************         
SNDCFNOT NTR1  BASE=*,LABEL=*                                                   
K        USING RDARKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'RDARKEY),SVDARKEY     READ 51 RECORD                       
         MVI   KEY,X'51'                                                        
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SNCF00   DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R6),IOWORK                                                      
*                                                                               
         USING RDARREC,R6          R6->DARE RECORD                              
*                                  R4->SPOOL AREA                               
         L     R7,AIO2                                                          
         USING RCONREC,R7                                                       
*                                  R3->CONT RECORD                              
SNCF05   DS    0H                                                               
*        LA    R4,R+1              LOAD A(EDICT RECORD)                         
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R4,ASPLAREA                                                      
         USING SPOOLD,R4                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPAR4     SET A(COMFACS)                               
         MVC   SPOOLBUF,ATIA       SET A(TIA)                                   
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLSUBID,=C'DCF'                                                  
*                                                                               
         MVC   PLUSER,=X'0011'    DEFAULT TO SJR                                
         OC    SVSSID,SVSSID                                                    
         BZ    *+10                                                             
         MVC   PLUSER,SVSSID       MOVE IN THE SENDING ID                       
*                                                                               
         MVC   PLDESC(2),=C'DC'                                                 
         MVC   PLDESC+3(8),QREPCON                                              
         MVI   PLCLASS,C'G'                                                     
*        OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
         MVI   SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
*                                  AND CLEAR ALL OTHER BITS                     
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,SNDCPRNT                                                      
         B     SNDC10                                                           
*                                                                               
SNDCPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R4)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SNDC10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
         DROP  R2                                                               
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         MVC   P1+4(5),=C'*HDR*'    HEADER RECORD                               
         MVC   P1+9(14),=CL14'EDICT=*DDSDARR'                                   
         MVI   P1+34,C'W'           WIDE REPORT                                 
         BAS   RE,SNDCPRNT                                                      
*                                                                               
* PRINT A ++DDS CARD                                                            
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'                                                     
         MVC   EDIPROG,=C'CNF'     TYPE=CONFIRM                                 
         MVC   EDIIDEN,=C'TRN'                                                  
*                                                                               
* INFORMATIONAL CHUNK FOR EDICT REPORTING                                       
                                                                                
         MVC   EDIRDRRP,RDARKREP   REP CODE                                     
         MVC   EDIRDRAG,RDARKAGY   AGENCY CODE                                  
         MVC   EDIRDRST,RDARKSTA   STATION CODE                                 
                                                                                
CONF30   DS    0H                                                               
         MVC   EDIRDRMD,RDARMEDI   MEDIA CODE                                   
                                                                                
         EDIT  RDAREST#,(3,EDIRDRES),ALIGN=LEFT                                 
* AGENCY ORDER #                                                                
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARKORD                                              
         EDIT  (P5,WORK+17),(8,EDIRDRAN),ALIGN=LEFT                             
                                                                                
* CONTRACT #                                                                    
         OC    RDARREP#,RDARREP#                                                
         BZ    CONF40                                                           
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(8,EDIRDRCN),ALIGN=LEFT                             
                                                                                
         MVC   EDIRDRSP,RCONSAL    SALESMAN CODE                                
                                                                                
CONF40   DS    0H                                                               
         MVC   EDIRDRBY,RDARBUYC   BUYER CODE                                   
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'02'        DESCRIPTIVE ELEMENT #2                       
         BRAS  RE,GETEL                                                         
         BNE   CONF50                                                           
         USING RDARCLEM,R6                                                      
         MVC   EDIRDRCL,RDARCLI    CLIENT CODE                                  
                                                                                
         MVC   EDIRDRP1,RDARPRD1   PRODUCT CODE 1                               
         MVC   EDIRDRP2,RDARPRD2   PRODUCT CODE 2                               
         DROP  R5,R6                                                            
                                                                                
CONF50   DS    0H                                                               
         BAS   RE,SNDCPRNT                                                      
         EJECT                                                                  
* ORDER CONFIRMATION LINE                                                       
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
*        GOTO1 VHEXOUT,DMCB,RDARKORD,TWAGORDN,4,=C'TOG'                         
                                                                                
         LA    R3,2                COUNT OUTPUT LINES START AT 2 FOR            
*                                    HEADER + TRAILER LINES                     
                                                                                
         LA    R5,P                                                             
         USING RORDCFMD,R5                                                      
         MVC   ROCFTID,=C'ORDCFM'                                               
         MVC   ROCFORDR,ERRORD#    AGENCY ORDER NUMBER                          
         MVC   ROCFFRID,RDARRCVR   ID OF SENDER                                 
         MVC   ROCFTOID,RDARSNDR   ID OF RECEIVER                               
*                                                                               
         GOTO1 VDATCON,DMCB,(5,WORK),(X'20',ROCFDATE)                           
*                                                                               
         ZAP   WORK(4),=P'0'                                                    
         THMS  DDSTIME=YES                                                      
         ST    R0,WORK             ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB              DDS TIME                                     
         AP    WORK(4),DUB(4)                                                   
*                                                                               
         CP    WORK(4),=P'240000'  PAST MIDNIGHT?                               
         BL    CONF60                                                           
         SP    WORK(4),=P'240000'  YES, BUMP TO NEXT DAY AND ADJUST             
         GOTO1 VADDAY,DMCB,ROCFDATE,(X'20',ROCFDATE),F'1'                       
*                                                                               
CONF60   DS    0H                                                               
         ICM   R1,15,WORK                                                       
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,ROCFTIME,2,0                                   
*                                                                               
         MVC   ROCFQSTA,RDARKSTA   STATION                                      
         CLI   ROCFQSTA+4,C'L'     IS IT LOW POWER?                             
         BE    CONF70              YES - LEAVE AS IS                            
         MVI   ROCFQSTA+5,C'V'     INSERT LAST CHAR OF MEDIA                    
         CLI   ROCFQSTA+4,C'T'     IS IT A TV STATION                           
         BE    CONF70              YES - LEAVE AS IS                            
         MVI   ROCFQSTA+5,C'M'     NO INSERT RADIO MEDIA                        
*&&DO                                                                           
         CLI   ROCFQSTA+4,C'A'     GET UID FROM STATION RECORD FOR              
         BE    CONF65              RADIO EDI                                    
         CLI   ROCFQSTA+4,C'F'     GET UID FROM STATION RECORD FOR              
         BNE   CONF70              RADIO EDI                                    
*                                                                               
CONF65   DS    0H                                                               
         GOTO1 =A(GETUID),DMCB,ROCFQSTA,RR=RELO                                 
*&&                                                                             
*                                                                               
* CONTRACT NUMBER                                                               
*                                                                               
CONF70   DS    0H                                                               
         GOTO1 VHEXOUT,DMCB,RDARREP#,ROCFRPCN,4,=C'TOG'                         
*                                                                               
* RETURN TO SENDER INFO                                                         
         MVC   ROCFRTRN,RDARRTS                                                 
         DROP  R5                                                               
                                                                                
         BAS   RE,SNDCPRNT                                                      
*                                                                               
         GOTO1 =A(ORDSAL),RR=RELO                                               
*                                                                               
* CONFIRM WITH COMMENTS, IF ANY                                                 
CONF75   DS    0H                                                               
         L     R6,ACONAREA                                                      
         USING RCFCREC,R6                                                       
         XC    0(32,R6),0(R6)                                                   
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,POWERCDE                                                
         MVC   RCFCKCON,RCONKCON                                                
         MVC   KEY,0(R6)                                                        
*         MVI   UPDATE,C'Y'                                                     
*         GOTO1 VHIGH                                                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(L'RCFCKEY),KEYSAVE                                           
         BNE   CONF120                                                          
*        MVI   UPDATE,C'Y'                                                      
*        GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R6),IOWORK                                                      
                                                                                
*                                                                               
         LA    R5,P                                                             
         USING RORDCOMD,R5                                                      
         MVC   ROCMTID,=C'ORDCOM'                                               
         MVC   ROCMORDR,ERRORD#                                                 
         MVC   ROCMBLIN,=C'0000'   GLOBAL COMMENT                               
         TM    RCFCIFLG,X'80'                                                   
         BZ    CONF80                                                           
         MVC   ROCMTEXT(L'MGOYMSG),MGOYMSG                                      
         B     CONF90                                                           
         DROP  R6                                                               
*                                                                               
CONF80   DS    0H                                                               
         MVC   ROCMTEXT(L'MGONMSG),MGONMSG                                      
*                                                                               
CONF90   DS    0H                                                               
         L     R6,ACONAREA                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CONF95                                                           
         USING RCFCIEL,R6                                                       
         CLC   RCFCIVER,RCFCIDVR   SKIP IF WE'VE SENT THIS ALREADY              
         BNH   CONF120                                                          
         MVC   RCFCIDVR,VERSION    SET VERSION NUMBER SO WE DON'T               
*                                  SEND THIS AGAIN ON NEXT CONFIRM              
CONF95   DS    0H                                                               
         L     R6,ACONAREA                                                      
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    CONF100                                                          
         BAS   RE,SNDCPRNT                                                      
         LA    R3,1(R3)            BUMP COUNTER                                 
         B     CONF120                                                          
*                                                                               
CONF100  DS    0H                                                               
         MVI   ROCMCONT,C'*'                                                    
         BAS   RE,SNDCPRNT                                                      
         LA    R3,1(R3)            BUMP COUNTER                                 
*                                                                               
         MVC   ROCMTID,=C'ORDCOM'                                               
         MVC   ROCMORDR,ERRORD#                                                 
         MVC   ROCMBLIN,=C'0000'   GLOBAL COMMENT                               
         USING RCFCTEL,R6                                                       
         CLI   RCFCTLEN,3          MIN LENGTH                                   
         BL    CONF110                                                          
         ZIC   R1,RCFCTLEN                                                      
         CLI   RCFCTLEN,73                                                      
         BL    *+8                                                              
         LA    R1,72               MAX LENGTH                                   
*                                                                               
         SH    R1,=H'3'            OVERHEAD + 1                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ROCMTEXT(0),RCFCTEXT                                             
*                                                                               
CONF110  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    CONF100                                                          
         BAS   RE,SNDCPRNT                                                      
         LA    R3,1(R3)            BUMP COUNTER                                 
*                                  UPDATE CFC REC WITH NEW VERSION NUM          
                                                                                
         L     R6,ACONAREA                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               (R6),IOWORK                                                      
*        GOTO1 VPUTREC,DMCB,IOAREA                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* ORDER CONFIRMATION LINE NUMBER EQUIVALENTS LINE                               
CONF120  DS    0H                                                               
*&&DO                                                                           
         GOTO1 =A(BUILDBUY),RR=RELO BUILD BUY ARRAY IN IO4                      
         L     R6,ACONAREA                                                      
                                                                                
         OC    0(2,R6),0(R6)       CHECK IF NO BUYS                             
         BZ    CONF160                                                          
                                                                                
         LA    R5,P                                                             
         USING RORDLIND,R5                                                      
         XC    P,P                 CLEAR PRINT LINE                             
                                                                                
CONF130  DS    0H                                                               
         MVC   ROLNTID,=C'ORDLIN'                                               
         MVC   ROLNORDR,ERRORD#                                                 
                                                                                
* CONSTRUCT A LINE OF AGENCY BUY AND REP BUY LINE NUMBERS                       
         EDIT  (1,0(R6)),(4,ROLNBLIN),FILL=0        AGENCY                      
         LA    R2,ROLNRLIN                                                      
                                                                                
CONF140  DS    0H                  REP BUY LINE NUMBERS                         
         EDIT  (1,1(R6)),(4,0(R2)),FILL=0                                       
         CLC   0(1,R6),2(R6)                                                    
         BNE   CONF150                                                          
         LA    R2,L'ROLNBLIN(R2)                                                
         LA    RF,ROLNRLIN+L'ROLNRLIN                                           
         CR    R2,RF                                                            
         BNL   CONF150                                                          
         LA    R6,2(R6)                                                         
         B     CONF140                                                          
                                                                                
CONF150  DS    0H                                                               
         LA    R3,1(R3)            ONE MORE FOR COUNTER                         
         OC    2(2,R6),2(R6)       CHECK IF NO MORE                             
         BZ    CONF160                                                          
         MVI   ROLNCONT,C'*'       ELSE SIGNFY MORE TO COME                     
         BAS   RE,SNDCPRNT                                                      
         LA    R6,2(R6)                                                         
         B     CONF130                                                          
                                                                                
CONF160  DS    0H                                                               
         BAS   RE,SNDCPRNT                                                      
         DROP  R5                                                               
         EJECT                                                                  
*&&                                                                             
* ORDER TRAILER LINE                                                            
CONF170  DS    0H                                                               
         LA    R5,P                                                             
         USING RORDTLRD,R5                                                      
         MVC   ROTRTID,=C'ORDTLR'                                               
         MVC   ROTRORDR,ERRORD#    AGENCY ORDER NUMBER                          
         EDIT  (R3),(6,ROTRRCCT),FILL=0                                         
         BAS   RE,SNDCPRNT                                                      
         DROP  R5                                                               
                                                                                
CONFCLOS DS    0H                                                               
         MVI   SPMODE,X'FF'        YES, CLOSE PRINTQ ENTRY                      
         GOTO1 ASPOOL,DMCB,(R4)                                                 
*                                                                               
SNDCX    EQU   *                                                                
         B     EXIT                                                             
         DROP  R7                                                               
*        DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*********************************************************************           
*                                                                               
*   GETUID:  RETRIEVE UID FROM STATION RECORD                                   
*                                                                               
*   PARM 1: 6-CHAR STATION CALL. WILL BE REPLACED WITH UID ON EXIT              
*                                                                               
*********************************************************************           
GETUID   NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         L     R3,0(R1)                                                         
*                                                                               
         LA    R6,KEY                                                           
         USING RSTAKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   RSTAKREP,POWERCDE                                                
         MVC   RSTAKSTA,0(R3)                                                   
         CLI   RSTAKSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
         DROP  R6                                                               
*                                                                               
*        GOTO1 VHIGH                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*        GOTO1 VGETREC,DMCB,(R4)                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R4),IOWORK                                                      
*                                                                               
         LR    R6,R4                                                            
         USING RSTAUIEL,R6                                                      
         MVI   ELCODE,X'2A'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(L'ROCFQSTA,R3),RSTAUIST                                        
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
MGOYMSG  DC  C'**PLEASE EXPECT A MAKEGOOD OFFER TO FOLLOW**'                    
MGONMSG  DC  C'**PLEASE PROCESS THE CHANGES DESCRIBED BELOW**'                  
***********************************************************************         
* SEND ORDSAL MESSAGE FOR RADIO EDI ORDERS                                      
***********************************************************************         
ORDSAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    P,P                 CLEAR PRINT LINE                             
         LA    R2,P                                                             
         USING RORDSALD,R2                                                      
         MVC   ROSPTID,=C'ORDSAL'                                               
         MVC   ROSPORDR,ERRORD#    AGENCY ORDER NUMBER                          
*                                                                               
         LA    R6,IOAREA           CHECK IF WE NEED TO SEND                     
         MVI   ELCODE,X'0A'        SALESPERSON/POINTPERSON                      
         BRAS  RE,GETEL                                                         
         BNE   ORDSAL10                                                         
         USING RDARPPEL,R6                                                      
         CLC   RDARPPSP,SPACES                                                  
         BE    ORDSAL10                                                         
*                                                                               
         GOTO1 =A(GETSALNM),RR=RELO                                             
         MVC   ROSPSALP,RDARPPSP                                                
         MVC   ROSPSALN,WORK                                                    
         DROP  R6                                                               
*                                                                               
ORDSAL10 DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
         CLI   RDARBSTS,C'A'       OPENED?                                      
         BE    ORDSAL20                                                         
         MVI   ROSPOKCF,C'Y'       CONFIRMING W/O PREVIOUS OPEN                 
         B     ORDSAL30                                                         
*                                                                               
ORDSAL20 DS    0H                                                               
         OC    ROSPSALP,ROSPSALP                                                
         BZ    ORDSALX             NO SALESPERON NOR FLAG TO SEND               
*                                  JUST SKIP MESSAGE                            
ORDSAL30 DS    0H                                                               
         GOTO1 ASPOOL,DMCB,(R4)                                                 
         MVI   LINE,2              FORCE EVERYTHING TO PAGE ONE                 
         AHI   R3,1                BUMP COUNTER                                 
*                                                                               
ORDSALX  DS    0H                                                               
         XIT1  REGS=(R3)           RETURN RECORD COUNTER                        
         DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
*&&DO                                                                           
*********************************************************************           
* READ CONTRACT BUY RECORDS AND BUILD BUY ARRAY                                 
* 255 X 2 BYTES                                                                 
* BYTE 1 = AGENCY BUY LINE NUMBER                                               
* BYTE 2 = CONTRACT BUY LINE NUMBER                                             
*********************************************************************           
BUILDBUY NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         L     R6,ACONAREA                                                      
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF  0(R6),(RF)          CLEAR SORT AREA                              
         SR    R5,R5               SORT RECORD COUNTER                          
                                                                                
         USING RBUYREC,R4                                                       
         XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,POWERCDE                                                
         MVC   RBUYKCON,QREPCON                                                 
         MVC   KEY,RBUYKEY                                                      
                                                                                
*        GOTO1 VHIGH                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(22),KEYSAVE                                                  
         BNE   BUILDX                                                           
                                                                                
BUILD10  DS    0H                                                               
*        GOTO1 VGETREC,DMCB,RBUYREC                                             
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R4),IOWORK                                                      
         OC    RBUYTSPT,RBUYTSPT   BUY HAS ZERO SPOTS                           
         BNZ   BUILD20                                                          
         TM    RBUYRTS,X'01'+X'20' BUY WAS ZEROED OUT, SKIP                     
         BNZ   BUILDSEQ                                                         
                                                                                
BUILD20  DS    0H                                                               
         OC    RBUYAGBL,RBUYAGBL                                                
         BZ    BUILDSEQ                                                         
         MVC   0(1,R6),RBUYAGBL    AGENCY ORDER BUY LINE                        
         MVC   1(1,R6),RBUYKLIN    REP BUY LINE                                 
         LA    R6,2(R6)                                                         
         LA    R5,1(R5)            # OF RECORDS                                 
                                                                                
BUILDSEQ DS    0H                                                               
*        GOTO1 VSEQ                                                             
         GOTO1 VDATAMGR,DMCB,(X'80',DMRSEQ),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(22),KEYSAVE                                                  
         BE    BUILD10                                                          
                                                                                
         L     R6,ACONAREA                                                      
         GOTO1 =V(XSORT),DMCB,(R6),(R5),2,1,0                                   
                                                                                
BUILDX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
GETSALNM NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
*                                                                               
         LA    R6,IOAREA           CHECK IF WE NEED TO SEND                     
         MVI   ELCODE,X'0A'        SALESPERSON/POINTPERSON                      
         BRAS  RE,GETEL            FOR BOTH OPEN/REJECT                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RDARPPEL,R6                                                      
*                                                                               
         XC    KEY,KEY             GET SALESPERSON NAME                         
         XC    WORK,WORK                                                        
         LA    R3,KEY                                                           
         USING RSALREC,R3                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,POWERCDE                                                
         MVC   RSALKSAL,RDARPPSP                                                
*                                                                               
         LA    R6,IOAREA           CHECK IF UNWIRED                             
         MVI   ELCODE,X'0F'        YES, NEED TO GET POINTPERSON                 
         BRAS  RE,GETEL            FOR BOTH OPEN/REJECT                         
         BNE   GSAL10                                                           
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRED?                                     
         BZ    GSAL10                                                           
         DROP  R6                                                               
*                                                                               
         MVI   RSALKTYP,X'31'                                                   
         DROP  R3                                                               
*                                                                               
GSAL10   DS    0H                                                               
*        GOTO1 VHIGH                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*        GOTO1 VGETREC,DMCB,(R4)                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               (R4),IOWORK                                                      
         USING RSALREC,R4                                                       
         MVC   WORK(L'RSALNAME),RSALNAME                                        
         DROP  R4                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
*    AGYHDR:                                                                    
***********************************************************************         
AGYHDR00 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    GROSPOTS,GROSPOTS   CLEAR GROSS SPOTS ACCUM                      
         XC    GROSDOLS,GROSDOLS   CLEAR GROSS $$ ACCUM                         
         XC    ERRBLOCK(ERRBLKLQ),ERRBLOCK                                      
         XC    SAVECON#,SAVECON#   CLEAR CONTRACT# SAVE AREA                    
         XC    OLDMEDI,OLDMEDI                                                  
         XC    BUYCOUNT,BUYCOUNT   BUY COUNTER                                  
         MVI   BUYHIFLG,0          BUY TOO HIGH FLAG                            
         MVI   ABUYNDX,1           INIT XML AGENCY BUY NUMBER                   
*                                                                               
         MVC   INPROG,6(R3)        LOAD RECORD IN PROGRESS                      
         MVC   LASTREC,6(R3)       SET  LAST RECORD TYPE                        
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYHDRD,R4                                                      
*&&DO                                                                           
* CODE MOVED TO DDEDIDARR/S (SKUI 3/27/07)                                      
* SET UP FOR CC'ING EDI TO MO FOR EAGLE ATLANTA (ETV)                           
*                                                                               
         MVI   MQFLAGS,0                                                        
         CLC   =C'ETVNY',PAHDTOID                                               
         BE    AHDR0003                                                         
         CLC   =C'ROBAT',PAHDTOID                                               
         BNE   AHDR0005                                                         
AHDR0003 EQU   *                                                                
         MVI   MQFLAGS,X'80'       YES, SEND THIS VIA MQ TO MO                  
         XC    MQOFFSET,MQOFFSET                                                
*                                                                               
         L     RE,AMQAREA          PUT MSG TO BUFFER FOR MQPUT                  
         LA    RF,L'MQAREA                                                      
         XCEFL                                                                  
         L     R3,AMQAREA                                                       
         MVC   0(16,R3),=CL16'MEDIAOCEANEDI***'                                 
*        MVC   0(16,R3),=CL16'DDSTESTINGQUEUE*'                                 
         L     RF,MQOFFSET                                                      
         LA    RF,16(RF)                                                        
         ST    RF,MQOFFSET                                                      
*&&                                                                             
AHDR0005 EQU   *                                                                
         MVC   QREPCON,PAHDRPCN    SAVE REP CONTRACT NUMBER                     
         MVC   QRETURN,PAHDRTRN    SAVE RETURN DATA                             
         MVC   ERRORD#,PAHDORDR    SAVE ORDER NUMBER FOR ERRORS                 
         MVC   ERRFRID,PAHDFRID    SAVE FROM ID FOR ERRORS                      
         MVC   ERRTOID,PAHDTOID    SAVE TO   ID FOR ERRORS                      
         MVC   ERRRCDT,PAHDDATE    SAVE RECEIVED DATE FOR ERRORS                
         MVC   ERRRCTM,PAHDTIME    SAVE RECEIVED TIME FOR ERRORS                
*                                  VALIDATE IF STATION CALL EXISTS              
*                                                                               
* IF HAWORTH SENDS ORDER TO TELEMUNDO(B1), ADD A MILLION                        
* TO THE CONTRACT NUMBER IF CONTRACT NUMBER IS LESS THAN A                      
* MILLION                                                                       
*                                                                               
         CLC   =C'B1',POWERCDE                                                  
         BNE   AHDR0006                                                         
         CLC   =C'HM',PAHDROUT                                                  
         BNE   AHDR0006                                                         
         CLC   =C'00000000',PAHDRPCN                                            
         BE    AHDR0006                                                         
         CLC   =C'00',PAHDRPCN                                                  
         BNE   AHDR0006                                                         
         MVI   PAHDRPCN+1,C'1'                                                  
         MVI   QREPCON+1,C'1'                                                   
*                                                                               
AHDR0006 EQU   *                                                                
*                                                                               
         CLC   =C'FN',POWERCDE     DESTINATION TO FTS?                          
         BNE   *+14                                                             
         MVC   ERRCODE,=H'5'       SET ERROR, UNKNOWN DEST ID                   
         B     EXIT                                                             
*                                                                               
         GOTO1 =A(VALIRTNS),DMCB,QVSTA,PAHDQSTA,RR=RELO                         
         BNZ   EXIT                ERROR ENCOUNTERED!                           
*                                                                               
         XCEFL IOAREA,4000                                                      
         L     RE,AIO2                                                          
         LA    RF,4000                                                          
         XCEFL                                                                  
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          FIRST BUILD KEY                              
*                                                                               
         MVI   RDARKTYP,X'41'      INSERT RECORD TYPE                           
         MVC   RDARKREP,POWERCDE   INSERT POWER CODE                            
         MVC   RDARKSTA,PAHDQSTA   INSERT STATION                               
         MVI   RDARKSTA+5,C' '     CLEAR LAST CHAR OF STATION FIELD             
         MVC   RDARKAGY,PAHDROUT   INSERT ROUTING AGENCY                        
         MVC   RDARKAOF,PAHDROUT+3 INSERT ROUTING AGENCY OFFICE                 
         GOTO1 VHEXIN,DMCB,PAHDORDR,RDARKORD,8,0                                
*                                  CONVERT ORDER NUMBER TO HEXIN                
         XC    SVDARKEY,SVDARKEY                                                
         MVC   SVDARKEY(L'RDARKEY),RDARKEY                                      
*                                                                               
* CHECK IF DARE+ XML ORDER                                                      
*                                                                               
         NI    DARFLAGS,X'FF'-DFXML                                             
         CLI   PAHDREVN,C'+'                                                    
         BNE   *+8                                                              
         OI    DARFLAGS,DFXML                                                   
*                                                                               
         GOTOR DELETOLD            DELETE EXISTING RECORDS                      
         BZ    AHDR0010            NOT FOUND, TRY OLD STATION CALL              
*                                                                               
         OC    ERRCODE,ERRCODE     ERROR TRIGGERED, EXIT                        
         BNZ   AHDRX                                                            
*                                                                               
         GOTOR SVCFSP              SAVE CONFIRM CONTRACT S/P CODE               
*                                                                               
         CLC   PAHDOLDS,SPACESX    IF ANY                                       
         BE    AHDR0010                                                         
*                                                                               
         XC    RDARKEY,RDARKEY                                                  
         MVI   RDARKTYP,X'41'      INSERT RECORD TYPE                           
         MVC   RDARKREP,POWERCDE   INSERT POWER CODE                            
         MVC   RDARKSTA,PAHDOLDS   INSERT OLD STATION                           
         MVI   RDARKSTA+5,C' '     CLEAR LAST CHAR OF STATION FIELD             
         MVC   RDARKAGY,PAHDROUT   INSERT ROUTING AGENCY                        
         MVC   RDARKAOF,PAHDROUT+3 INSERT ROUTING AGENCY OFFICE                 
         GOTO1 VHEXIN,DMCB,PAHDORDR,RDARKORD,8,0                                
*                                  CONVERT ORDER NUMBER TO HEXIN                
*        BAS   RE,PURGEOLD         IF OLD CALLS FOUND, DELETE OLD CALLS         
         GOTO1 =A(PURGEOLD),RR=RELO                                             
*                                  AND USE NEW STATION CALLS                    
*                                  THIS SHOULD *NEVER* HAPPEN                   
*                                  IF SO, ALL STATUS WILL BE LOST               
         MVC   KEYSAVE+9(L'RDARKSTA),PAHDQSTA                                   
         MVI   KEYSAVE+14,C' '     CLEAR LAST CHAR OF STATION                   
*                                                                               
AHDR0010 EQU   *                                                                
         XCEFL IOAREA,4000         CLEAR THE IOAREA AGAIN                       
*        MVC   RDARKEY,KEYSAVE     RESET FIRST PART OF RECORD                   
         MVC   RDARKEY,SVDARKEY    RESET FIRST PART OF RECORD                   
         MVI   RDARKRT,X'10'       INSERT RECORD TYPE                           
         LA    RF,RDARAGLN+RDARCLLN+34                                          
*                                  SET RECORD LENGTH                            
         STCM  RF,3,RDARLEN        INSERT INTO RECORD                           
         MVI   RDARELEM,X'01'      INSERT FIRST ELEMENT TYPE                    
         LA    RF,RDARAGLN                                                      
         STC   RF,RDARELLN         INSERT ELEMENT LENGTH                        
         MVI   RDARCLEM,X'02'      INSERT SECOND ELEMENT TYPE                   
         LA    RF,RDARCLLN                                                      
         STC   RF,RDARCLEN         INSERT ELEMENT LENGTH                        
         MVC   RDARVER#,PAHDVERS   INSERT VERSION NUMBER                        
         MVC   RDARSNDR,PAHDFRID   INSERT SENDER ID                             
         MVC   RDARRCVR,PAHDTOID   INSERT RECEIVER ID                           
         GOTO1 VDATCON,DMCB,(0,PAHDDATE),(2,RDARDATE)                           
*                                  INSERT DATE (COMPRESSED)                     
         PACK  DUB(8),PAHDTIME(4)                                               
*                                  CONVERT AND STORE TIME                       
         CVB   RF,DUB                                                           
         STCM  RF,3,RDARTIME       2 CHARS OUTPUT                               
         MVC   RDARMEDI,PAHDQMED   INSERT MEDIA                                 
         MVC   OLDMEDI,PAHDQMED    SAVE OFF MEDIA                               
         LA    RF,PAHDQEST+5       A(LAST POSITION OF FIELD)                    
         LA    R0,6                MAX SIZE OF FIELD                            
*                                                                               
*   NEED TO CALCULATE LENGTH OF INPUT FIELD BY COUNTING TRAILING                
*      SPACES                                                                   
*                                                                               
AHDR0020 EQU   *                                                                
         CLI   0(RF),X'40'         FIELD = SPACE?                               
         BNE   AHDR0030            NO  - FINISHED                               
         BCTR  RF,0                BACK UP 1 POSITION                           
         BCT   R0,AHDR0020         YES - GO BACK FOR NEXT                       
*                                                                               
         MVC   ERRCODE,=H'304'     SET ERROR, ESTIMATE NOT FOUND                
         B     AHDR0100                                                         
*        DC    H'0'                NO ESTIMATE NUMBER - !!!                     
AHDR0030 EQU   *                                                                
         LR    RF,R0               LOAD ANOTHER REGISTER                        
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAHDQEST(0),=C'000000'                                           
         BL    AHDR0040                                                         
*                                  SKIP IF NON-NUMERIC                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAHDQEST(0),=C'999999'                                           
         BH    AHDR0040                                                         
*                                  SKIP IF NON-NUMERIC                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),PAHDQEST(0)  PACK BY LENGTH                               
*                                  CONVERT AND STORE ESTIMATE #                 
         CVB   RF,DUB                                                           
         STCM  RF,7,RDAREST#       3 CHARS OUTPUT                               
*                                                                               
AHDR0040 EQU   *                                                                
         BAS   RE,AHDRSDAT         SET FLIGHT START DATE                        
         GOTO1 VDATCON,DMCB,(0,PAHDESTN),(2,RDARESEN)                           
*                                  INSERT ESTIMATE END   DATE                   
         MVC   HIASTART,RDARESST   SAVE FLIGHT START DATE                       
         MVC   HIAEND,RDARESEN     SAVE FLIGHT END   DATE                       
         MVC   SVESEND,RDARESEN                                                 
*                                     FOR HIATUS TESTING                        
*                                                                               
         OC    PAHDRPCN,SPACESX    SET SPACES IF BINARY                         
         CLC   PAHDRPCN,=8C'0'     HAVE A CONTRACT NUMBER TO CHECK?             
         BNH   AHDR0060                                                         
*                                                                               
* ASSUMES CONTRACT NUMBER MUST BE RIGHT JUSTIFIED WITH LEADING ZEROES           
* CHECK TO MAKE SURE ONLY VALID NUMERIC DIGITS ARE SENT                         
*                                                                               
         LA    RE,8                                                             
         LA    RF,PAHDRPCN                                                      
*                                                                               
AHDR0041 EQU   *                                                                
         CLI   0(RF),C'0'                                                       
         BL    AHDR0042                                                         
         CLI   0(RF),C'9'                                                       
         BH    AHDR0042                                                         
         AHI   RF,1                                                             
         BCT   RE,AHDR0041                                                      
         B     AHDR0043                                                         
*                                                                               
AHDR0042 EQU   *                                                                
         MVC   ERRCODE,=H'909'     SET ERROR, BAD REP CONTRACT NUMBER           
         B     AHDR0100                                                         
*                                                                               
AHDR0043 EQU   *                                                                
         NI    DARFLAGS,X'FF'-DFMGCHK                                           
*                                                                               
         ZAP   WORK(8),=P'99999999'                                             
         PACK  DUB,PAHDRPCN                                                     
         SP    WORK(8),DUB(8)                                                   
         L     R0,WORK                                                          
         L     R1,WORK+4                                                        
         SRDL  R0,4                                                             
         ST    R1,FULL             9'S COMPLEMENT CONTRACT NUMBER               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         MVI   RCONPTYP,X'8C'      CONTRACT PASSIVE PTR 1                       
         MVC   RCONPREP,POWERCDE                                                
         MVC   RCONPCON,FULL                                                    
*                                                                               
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   AHDR0045            CAN'T FIND CONTRACT FOR THIS REP             
         BAS   RE,CKCONREC         MAKE SURE DARE LINK IS VALID                 
         BE    AHDR0060            FOUND A CONTRACT FOR THIS REP                
*                                                                               
         OC    ERRCODE,ERRCODE                                                  
         BNZ   AHDRX                                                            
*                                                                               
** WE DON'T HAVE A KEY FOR THIS CONTRACT NUMBER, SEE IF TAKEOVER                
AHDR0045 XC    KEY,KEY                                                          
         MVI   RCONTTYP,X'AD'      PASSIVE FOR DARE TAKEOVER                    
         MVC   RCONTREP,POWERCDE                                                
         MVC   RCONTSTA,RDARKSTA                                                
*                                                                               
         CLI   RCONTSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   RCONTSTA+4,C' '                                                  
*                                                                               
         GOTO1 VHEXIN,DMCB,PAHDRPCN,RCONTOLD,8                                  
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
         CLC   KEYSAVE(RCONTCON-RCONKEY),KEY                                    
         BNE   AHDR0050                                                         
         BAS   RE,CKCONREC         MAKE SURE DARE LINK IS VALID                 
         BNE   AHDR0050                                                         
         MVC   RDARREP#,RCONTCON                                                
AHDR0047 GOTO1 VHEXOUT,DMCB,RDARREP#,PAHDRPCN,4                                 
         MVC   QREPCON,PAHDRPCN                                                 
         B     AHDR0065                                                         
         DROP  R6                                                               
*                                                                               
AHDR0050 DS    0H                                                               
*** SPECIAL FOR MG DUE TO ALL/MG FILE MERGE                                     
         TM    DARFLAGS,DFMGCHK    HAVE WE DONE THIS ALREADY?                   
         BO    AHDR0055                                                         
         OI    DARFLAGS,DFMGCHK                                                 
         CLC   =C'MG',POWERCDE                                                  
         BNE   AHDR0055                                                         
         CLC   =C'00',PAHDRPCN                                                  
         BNE   AHDR0055                                                         
         MVI   PAHDRPCN+1,C'1'     TRY AGAIN                                    
         B     AHDR0043                                                         
*                                                                               
AHDR0055 DS    0H                  BAD CONTRACT, CLEAR AND CONTINUE             
         XC    RDARREP#,RDARREP#                                                
         B     AHDR0047                                                         
***********************************************************************         
* INSERT REP CONTRACT NUMBER                                                    
***********************************************************************         
AHDR0060 GOTO1 VHEXIN,DMCB,PAHDRPCN,RDARREP#,8                                  
*                                                                               
* AGENCY SIDE RESEND ORDER WITH NO CONTRACT NUMBER WHILE REPPAK ORDER           
* HAS THE CONTRACT # IN THE RECORD. IN THIS CASE, WE WILL PUT THE               
* CONTRACT NUMBER BACK IN THE ORDER RECORD                                      
*                                                                               
AHDR0065 DS    0H                                                               
         OC    RDARREP#,RDARREP#                                                
         BNZ   AHDR0070                                                         
         MVC   RDARREP#,SAVECON#                                                
*                                                                               
AHDR0070 DS    0H                                                               
         MVC   RDARRTS,PAHDRTRN    INSERT 'RETURN TO SENDER' INFO               
         MVC   RDARCORT,PAHDCTRD   INSERT 'CASH/TRADE' FLAG                     
         MVC   RDARCLI,PAHDQCLT    INSERT CLIENT CODE                           
         MVC   RDARPRD1,PAHDQPRD   INSERT 1ST PRODUCT CODE                      
         OC    PAHDQPR2,SPACESX    SET SPACES IF BINARY                         
         MVC   RDARPRD2,PAHDQPR2   INSERT 2ND PRODUCT CODE                      
         CLI   RESENTFL,C'N'       HAS ORDER BEEN RESENT?                       
         BE    AHDR0080            NO  - DON'T SET RESENT FLAG                  
*                                     IN RECORD                                 
         OI    RDARMISC,X'80'      YES - SET RESENT FLAG                        
         TM    SAVEMISC,X'40'      WAS ORDER PREVIOUSLY APPROVED?               
         BNO   AHDR0080            NO                                           
         OI    RDARMISC,X'40'      YES - SET PREV APPROVED FLAG                 
*                                                                               
AHDR0080 EQU   *                                                                
         MVI   REVNUM,0            INIT                                         
         CLC   PAHDREVN,SPACESX    REVISION??                                   
         BE    AHDR0083                                                         
*                                                                               
* CHECK IF DARE+ XML ORDER                                                      
*                                                                               
         NI    DARFLAGS,X'FF'-DFXML                                             
         CLI   PAHDREVN,C'+'                                                    
         BNE   AHDR0082                                                         
         OI    DARFLAGS,DFXML                                                   
         MVI   PAHDREVN,C' '                                                    
         CLC   PAHDREVN+1(2),SPACESX                                            
         BE    AHDR0083                                                         
         MVI   PAHDREVN,C'0'                                                    
*                                                                               
AHDR0082 EQU   *                                                                
         PACK  DUB(8),PAHDREVN(3)                                               
*                                  CONVERT AND STORE REVISION NUMBER            
*                                  ONE CHARACTER SHOULD BE SUFFICIENT           
         CVB   RF,DUB                                                           
         STC   RF,RDARRNUM                                                      
         MVC   REVNUM,RDARRNUM                                                  
         CLC   PREVNUM,RDARRNUM    IF REVISION NUMBER IS DIFFERENT              
         BE    AHDR0083            THE REVISION IS NOT A RESEND                 
         NI    RDARMISC,X'FF'-X'80'                                             
*                                                                               
AHDR0083 EQU   *                                                                
         CLC   =C'VAR',PAHDTID     IS THIS A VARIOUS ORDER?                     
         BNE   AHDR0087                                                         
*        BE    AHDR0084                                                         
*        CLC   PAHDREVN,SPACESX    IS THIS A REVISION ORDER?                    
*        BNE   AHDR0087                                                         
*        B     AHDR0090                                                         
*                                                                               
AHDR0084 EQU   *                                                                
         TM    SAVEMISC,X'10'      WAS ORDER VARIOUS PREVIOUSLY?                
         BO    AHDR0085            NO, THIS IS A VARNEW, NOT RESENT             
         NI    RDARMISC,X'FF'-X'80'                                             
*                                                                               
AHDR0085 EQU   *                                                                
         OI    RDARMISC,X'10'      MARK ORDER AS VARIOUS                        
*                                                                               
AHDR0087 EQU   *                                                                
         OC    RDARREP#,RDARREP#                                                
         BZ    AHDR0090                                                         
         GOTO1 =A(MARKCON),RR=RELO                                              
*                                                                               
AHDR0090 EQU   *                                                                
*&&DO                                                                           
         CLC   PAHDVARN,SPACESX                                                 
         BE    AHDR0100                                                         
         CLC   PAHDVARN,=8C'0'                                                  
         BE    AHDR0100                                                         
         OI    RDARMISC,X'08'      ORDER IS BRAND                               
         MVI   ELTAREA,X'0B'                                                    
         MVI   ELTAREA+1,RDARBRLQ                                               
         GOTO1 VHEXIN,DMCB,PAHDVARN,ELTAREA+2,8,0                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA                    
*                                                                               
         GOTO1 =A(VARIOUS),RR=RELO                                              
*&&                                                                             
*                                                                               
*        ALL FIELDS FROM AGYHDR RECORD LOADED TO NEW FORMAT.                    
*        NOTHING TO WRITE OUT AT THIS TIME.                                     
*                                                                               
AHDR0100 DS    0H                                                               
         TM    DARFLAGS,DFXML                                                   
         BO    AHDR0110                                                         
         CLI   RDARKSTA+4,C'A'                                                  
         BE    AHDR0110                                                         
         CLI   RDARKSTA+4,C'F'                                                  
         BNE   AHDRX                                                            
*                                                                               
AHDR0110 DS    0H                                                               
         XC    ELTAREA,ELTAREA                                                  
ELTD     USING RDARFLEM,ELTAREA                                                 
         MVI   ELTD.RDARFLCD,X'0F'                                              
         MVI   ELTD.RDARFLLN,RDARFLLQ                                           
         TM    OLDFLG1,X'04'                                                    
         BZ    *+8                                                              
         OI    ELTD.RDARFLG1,X'04'  SAVE PRE-BOOK FLAG                          
*                                                                               
         TM    DARFLAGS,DFXML                                                   
         BZ    *+8                                                              
         OI    ELTD.RDARFLG2,X'80'  XML FLAG                                    
*                                                                               
         MVC   ELTD.RDAROROT,PAHDSDAY                                           
         DROP  ELTD                                                             
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA                    
*                                                                               
AHDRX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* HAVE TO CHECK THE CONTRACT RECORD TO MAKE SURE THE DARE LINK IS THERE         
***********************************************************************         
CKCONREC NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,            X        
               AIO2,IOWORK                                                      
         NI    DARFLAGS,X'FF'-DFCONCF                                           
*                                                                               
         L     R6,AIO2                                                          
*                                                                               
         XC    VERSION,VERSION                                                  
         XC    SVSSID,SVSSID                                                    
         MVI   ELCODE,X'20'        GET SEND ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   CKCRC10                                                          
         USING RCONSEND,R6                                                      
         MVC   VERSION,RCONSRV                                                  
         MVC   SVSSID,RCONSSID                                                  
         DROP  R6                                                               
*                                                                               
CKCRC10  DS    0H                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'1F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CKCRC15X                                                         
         USING RCONXEL,R6                                                       
*                                                                               
* FOR XML ORDERS, ERROR IF CONTRACT HAS BEEN CONFIRMED                          
*                                                                               
         TM    DARFLAGS,DFXML                                                   
         BZ    CKCRC13                                                          
         TM    RCONCONF,X'40'+X'20'                                             
         BZ    CKCRC13                                                          
         MVC   ERRCODE,=H'305'     SET ERROR, ALREADY CONFIRMED                 
         B     CKCRCNO                                                          
*                                                                               
CKCRC13  DS    0H                                                               
         TM    RCONCONF,X'40'                                                   
         BZ    CKCRC15X                                                         
         OI    DARFLAGS,DFCONCF                                                 
         DROP  R6                                                               
CKCRC15X DS    0H                                                               
         L     R6,AIO2                                                          
         LA    R6,RCONELEM-RCONREC(R6)                                          
*                                                                               
CKCRC20  CLI   0(R6),0                                                          
         BE    CKCRCNO                                                          
         CLI   0(R6),X'1D'         DARE LINK ELEMENT PRESENT?                   
         BE    CKCRC30                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKCRC20                                                          
*                                                                               
         USING RCONDREL,R6                                                      
CKCRC30  GOTO1 VHEXIN,DMCB,PAHDORDR,FULL,8,0                                    
         CLC   RCONDRLK,FULL                                                    
         BNE   CKCRCNO             NO MATCH WITH THIS ORDER NUMBER              
CKCRCYES B     YES                                                              
CKCRCNO  B     NO                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                  INSERT ESTIMATE START DATE                   
*                                                                               
*   AHDRSDAT: CALCULATE ORDER FLIGHT START DATE BASED ON                        
*        ESTIMATE START DATE                                                    
*        SCHEDULE START (FIRST AIR DATE)                                        
*        OUT OF WEEK START DAY NUMBER                                           
*                                                                               
AHDRSDAT NTR1                                                                   
         XC    ELTAREA,ELTAREA     ADD FIRST AIR DATE                           
K        USING RDAREL2M,ELTAREA                                                 
         MVI   K.RDAREL2D,X'11'                                                 
         MVI   K.RDAREL2N,RDAR2MLQ                                              
         MVC   WORK(L'PAHDESTS),PAHDESTS                                        
         CLC   PAHDAIRD,SPACESX    NO FIRST AIR DATE, DO NOT SAVE               
         BE    *+10                SHOULD ONLY HAPPEN FOR REVISIONS             
         MVC   WORK(L'PAHDESTS),PAHDAIRD                                        
         GOTO1 VDATCON,DMCB,(0,WORK),(2,K.RDAR2FDT)                             
         MVC   K.RDAR2EST,PAHDQEST                                              
         DROP  K                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA,          X        
               =C'ADD=CODE'                                                     
*                                                                               
ASDA0005 EQU   *                                                                
*        CLC   PAHDREVN,SPACESX    REVISION ALWAYS USE EST START                
*        BNE   ASDA0010                                                         
         CLC   PAHDAIRD,SPACESX    NO FIRST AIR DATE, USE EST START             
         BE    ASDA0010            SHOULD ONLY HAPPEN FOR REVISIONS             
         CLC   PAHDESTS,PAHDAIRD   EST START = FIRST AIR DATE?                  
         BNE   ASDA0020            NO  -                                        
*                                                                               
ASDA0010 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(0,PAHDESTS),(2,RDARESST)                           
*                                  YES - INSERT ESTIMATE START DATE             
         B     ASDA0200            EXIT - FINISHED                              
ASDA0020 EQU   *                                                                
         GOTO1 VGETDAY,DMCB,PAHDAIRD,DUB                                        
         PACK  DUB(8),PAHDSDAY     PACK OUT OF WEEK ROTATOR                     
*                                     WILL ALWAYS BE 1-7                        
         CVB   RF,DUB              INSERT ROTATOR INTO REGISTER                 
         ZIC   RE,DMCB             RETRIEVE DAY FROM GETDAY                     
         CR    RF,RE               ROTATOR DAY VS AIR DAY:                      
         BH    ASDA0040            OOW > AIR DAY                                
         BL    ASDA0060            AIR DAY > OOW                                
         GOTO1 VDATCON,DMCB,(0,PAHDAIRD),(2,RDARESST)                           
*                                  AIR = OOW: USE SCHEDULE START DATE           
         B     ASDA0200            EXIT - FINISHED                              
ASDA0040 EQU   *                                                                
         LA    RE,7(RE)            SET AIR DAY > OOW DAY                        
ASDA0060 EQU   *                                                                
         SR    RE,RF               SUBTRACT OOW DAY FROM AIR DAY                
         LNR   RE,RE               INVERT THE VALUE                             
         ST    RE,DMCB+8           INSERT DAY ADJUSTMENT INTO P3                
         GOTO1 VADDAY,DMCB,PAHDAIRD,DUB                                         
         GOTO1 VDATCON,DMCB,(0,DUB),(2,RDARESST)                                
*                                  USE ADJUSTED SCHEDULE START DATE             
ASDA0200 EQU   *                                                                
         B     EXIT                                                             
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   AGYDS4:   AGENCY DESCRIPTION 4 RECORD                                       
*                                                                               
AGYDS400 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYDS4D,R4                                                      
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
*                                                                               
         MVC   RDARBUYC,PAD4BUYR   INSERT BUYER CODE                            
         MVC   RDARBUYR,PAD4BYRN   INSERT BUYER NAME                            
         MVC   RDARBTEL,PAD4BYRT   INSERT BUYER TELEPHONE #                     
         MVC   RDARBXTN,PAD4BYRX   INSERT BUYER EXTENSION                       
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*   AGYDS5:   AGENCY DESCRIPTION 5 RECORD                                       
*                                                                               
AGYDS500 NTR1  BASE=*,LABEL=*                                                   
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYDS5D,R4                                                      
*                                                                               
         CLI   PAD5UNWR,C'Y'       UNWIRED ORDER?                               
         BNE   AGYDS530                                                         
*                                                                               
         MVC   DATADISP,=H'34'     SET UP FOR REPFILE                           
         MVI   ELCODE,X'0F'                                                     
         LA    R6,IOAREA                                                        
         BRAS  RE,GETEL                                                         
         BE    AGYDS520                                                         
*                                                                               
         LA    R2,ELTAREA                                                       
         XC    ELTAREA,ELTAREA                                                  
         USING RDARFLEM,R2                                                      
         MVI   RDARFLCD,X'0F'                                                   
         MVI   RDARFLLN,RDARFLLQ                                                
         DROP  R2                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA                    
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   EXIT                SHOULD NEVER HAPPEN                          
*                                                                               
AGYDS520 DS    0H                                                               
         USING RDARFLEM,R6                                                      
         OI    RDARFLG1,X'01'                                                   
         DROP  R6                                                               
*                                                                               
AGYDS530 DS    0H                                                               
*                                                                               
         OC    PAD5SALP,PAD5SALP   SALESPERSON/POINTPERSON?                     
         BZ    EXIT                                                             
*                                                                               
         LA    R2,ELTAREA                                                       
         XC    ELTAREA,ELTAREA                                                  
         USING RDARPPEL,R2                                                      
         MVI   RDARPPCD,X'0A'                                                   
         MVI   RDARPPLN,RDARPPLQ                                                
         MVC   RDARPPSP,PAD5SALP                                                
         MVC   SPCODE,PAD5SALP     SAVE THIS OFF FOR AUDIT TRAIL                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA                    
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   AGYCAN:  AGENCY CANCEL RECORD                                               
*        THIS CAN BE 'NOTDARE' OR 'REVISION CANCEL' SITUATION                   
***********************************************************************         
AGYCAN00 NTR1  BASE=*,WORK=(R5,AGCWORKQ),LABEL=*                                
         USING AGCWORKD,R5         CLEAR WORK AREA                              
         XC    AGCWORKD(AGCWORKQ),AGCWORKD                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYCAND,R4                                                      
*                                                                               
* SAVE INCASE OF ERROR                                                          
*                                                                               
         MVC   QREPCON,PACNRPCN    SAVE REP CONTRACT NUMBER                     
         MVC   QRETURN,PACNRTRN    SAVE RETURN DATA                             
         MVC   ERRORD#,PACNORDR    SAVE ORDER NUMBER FOR ERRORS                 
         MVC   ERRFRID,PACNFRID    SAVE FROM ID FOR ERRORS                      
         MVC   ERRTOID,PACNTOID    SAVE TO   ID FOR ERRORS                      
         MVC   ERRRCDT,PACNDATE    SAVE RECEIVED DATE FOR ERRORS                
         MVC   ERRRCTM,PACNTIME    SAVE RECEIVED TIME FOR ERRORS                
*                                                                               
*                                                                               
         GOTO1 =A(VALIRTNS),DMCB,QVSTA,PACNQSTA,RR=RELO                         
         BNZ   EXIT                ERROR ENCOUNTERED!                           
*                                                                               
*                                                                               
*   SAVE ORDER REJECT IDENTIFYING FIELDS FOR 'NOTDARE' USE                      
*                                                                               
         MVC   ORORD#,PACNORDR     SAVE AGENCY ORDER NUMBER                     
         MVC   ORCON#,PACNRPCN     SAVE REP CONTRACT NUMBER                     
*        MVC   ORSTAT,PACNQSTA     SAVE STATION                                 
         MVC   ORAGY(5),PACNROUT   SAVE ROUTING CODE                            
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2                                                       
*                                                                               
         XC    RDARKEY,RDARKEY     CLEAR THE DARE KEY AREA                      
         MVI   RDARKTYP,X'41'      SET KEY TYPE 41                              
         MVC   RDARKREP,POWERCDE   INSERT REP ID                                
         MVC   RDARKSTA,ORSTAT     INSERT STATION                               
         MVI   RDARKSTA+5,C' '     CLEAR LAST CHAR                              
*                                                                               
         MVC   RDARKAGY(5),ORAGY   INSERT ROUTING CODE                          
         GOTO1 VHEXIN,DMCB,ORORD#,RDARKORD,8,0                                  
*                                  CONVERT ORDER NUMBER TO HEXIN                
*                                                                               
         MVI   RDARKRT,X'10'       INSERT RECORD TYPE = HEADER                  
         MVC   KEY(27),RDARKEY                                                  
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND? (ENTIRE KEY CHECKED)              
         BE    ACAN0050            YES                                          
*                                                                               
         CLC   PACNOLDS,SPACESX    NO, ANY OLD CALL LETTER?                     
         BNE   ACAN0040                                                         
         GOTO1 =A(SNDERROR),DMCB,928,RR=RELO                                    
         B     ACAN0900                                                         
*                                                                               
ACAN0040 EQU   *                                                                
         MVC   ORSTAT,PACNOLDS     SAVE STATION                                 
         MVC   RDARKSTA,ORSTAT     INSERT STATION                               
         MVI   RDARKSTA+5,C' '     CLEAR LAST CHAR                              
         MVC   KEY(27),RDARKEY                                                  
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND? (ENTIRE KEY CHECKED)              
         BE    ACAN0050            NO  - AGENCY ORDER NOT FOUND                 
         GOTO1 =A(SNDERROR),DMCB,911,RR=RELO                                    
         B     ACAN0900            SEND ERROR BACK TO AGENCY                    
*                                                                               
ACAN0050 EQU   *                                                                
         TM    KEY+27,X'80'                                                     
         BO    ACAN0900            ORDER ALREADY CANCELLED/DELETED              
*                                                                               
         MVC   AGCANDA,KEY+28                                                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,            X        
               IOAREA,IOWORK                                                    
*                                  READ THE RECORD                              
         CLI   PACNCFLG,C'R'       CANCEL FLAG SET FOR REVISION CANCEL?         
         BNE   ACAN0100                                                         
*                                                                               
         OI    RDARDELS,X'20'      FLAG REVISION CANCELLED                      
*                                                                               
         BAS   RE,RECWRITE                                                      
*                                  REWRITE PURGED REC                           
         GOTO1 =A(DEL41PAS),RR=RELO DELETE PASSIVE KEYS                         
         GOTO1 =A(PURGEOLD),RR=RELO     DELETE EXISTING RECORDS                 
*                                                                               
         GOTO1 =A(RESETREV),RR=RELO RESET REVISION NUMBER IN CONTRACT           
         B     ACAN0900                                                         
*                                                                               
ACAN0100 DS    0H                                                               
*HQ      CLI   RDARMEDI,C'R'       REBUILD KEYS FOR RADIO ONLY                  
*        BE    ACAN0110                                                         
*        OI    RDARMISC,X'20'      SET STATUS = NOTDARE                         
*        B     ACAN0120                                                         
*                                                                               
ACAN0110 DS    0H                                                               
         L     RE,APASAREA         CLEAR KEY BUILD AREA                         
         LA    RF,L'PASAREA                                                     
         XCEFL                                                                  
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',VCOMFACS),APASAREA,     X        
               IOAREA,ASPLAREA                                                  
*                                                                               
         OI    RDARMISC,X'20'      SET STATUS = NOTDARE                         
*                                                                               
         L     R3,APASAREA         CREATE NEW PASSIVE POINTER                   
         LA    R3,800(R3)                                                       
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',VCOMFACS),(R3),IOAREA,  X        
               ASPLAREA                                                         
*                                                                               
*   MODIFY THE DATE/TIME OF RECEIPT OF 'NOTDARE'                                
*                                                                               
ACAN0120 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,WORK),(2,RDARDATE)                               
*                                  FETCH TODAY'S DATE                           
         BAS   RE,TIMEFLAT         GET TIME MARKER                              
         MVC   RDARTIME,SAVTIME    INSERT TIME INTO RECORD                      
*                                                                               
         BAS   RE,RECWRITE         YES - WRITE THE FINAL RECORD                 
*                                                                               
* HQ     CLI   RDARMEDI,C'R'       REBUILD KEYS FOR RADIO ONLY                  
*        BNE   ACAN0900                                                         
*                                                                               
         L     R3,APASAREA         CREATE NEW PASSIVE POINTER                   
         LA    R3,800(R3)                                                       
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',VCOMFACS),APASAREA,(R3),X        
               AGCANDA                                                          
*                                                                               
ACAN0900 EQU   *                                                                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DEL41PAS:  DELETE PASSIVE KEYS FOR 41 RECORDS                                 
*                                                                               
DEL41PAS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,APASAREA         CLEAR KEY BUILD AREA                         
         LA    RF,L'PASAREA                                                     
         XCEFL                                                                  
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2                                                       
*                                                                               
         CLI   RDARKTYP,X'41'      SKIP READ IF DARE HEADER RECORD              
         BNE   DPAS06              ALREADY IN IOAREA                            
         CLI   RDARKRT,X'10'                                                    
         BE    DPAS10                                                           
*                                                                               
DPAS06   DS    0H                                                               
         MVC   KEY,RDARKEY                                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(24),KEY     FIRST KEY FOUND?                             
         BNE   DPASNO              NO  - SET CC TO NO                           
*                                                                               
DPAS08   EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
DPAS10   EQU   *                                                                
         LA    R2,IOAREA                                                        
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        APASAREA: KEY BUILD AREA                                               
*        R2  : CURRENT LOCATION OF AGENCY ORDER RECORD                          
*        ASPLAREA: IO AREA                                                      
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'81',VCOMFACS),APASAREA,(R2),X        
               ASPLAREA                                                         
*                                                                               
*   DELETE ALL 41 PASSIVES                                                      
*        APASAREA: KEY BUILD AREA                                               
*        R2  : CURRENT LOCATION OF AGENCY ORDER RECORD                          
*        ASPLAREA: IO AREA                                                      
*                                                                               
         L     R4,APASAREA         A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->ALL NULL                                 
         LHI   RF,800              THIS WILL DELETE ALL THE OLD PTS             
         XCEF  (R4)                                                             
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',VCOMFACS),APASAREA,(R4),X        
               HDRDA                                                            
DPASYES  SR    RC,RC                                                            
DPASNO   LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
* PURGEOLD:  PURGE OLD RECORDS, IF ANY, FOR THIS ORDER.  KEY                    
*        IS BUILT THROUGH ORDER NUMBER.                                         
*                                                                               
PURGEOLD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2                                                       
*                                                                               
         MVC   KEY,RDARKEY                                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(24),KEY     FIRST KEY FOUND?                             
         BNE   PURGNO              NO  - SET CC TO NO                           
*                                                                               
         B     PURG0040                                                         
PURG0020 EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,(X'88',DMRSEQ),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
PURG0040 EQU   *                                                                
         CLC   KEYSAVE(24),KEY     SAME KEY THROUGH ORDER NUMBER?               
         BNE   PURGYES             NO  - FINISHED                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                  YES - RETRIEVE RECORD                        
*                                                                               
*   RECORD PURGTE BIT NO LONGER BEING SET                                       
*                                                                               
         OI    RDARCNTL,X'80'      SET PURGTE BIT                               
*                                     OF ORIGINAL RECORD                        
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                  REWRITE PURGED REC                           
*                                                                               
         OI    KEY+27,X'80'        SET PURGE BIT IN KEY                         
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEYSAVE,KEY             
*                                  REWRITE PURGED KEY                           
         B     PURG0020            GO BACK FOR NEXT                             
*                                                                               
PURGYES  SR    RC,RC                                                            
PURGNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* SVCFSP  :  SAVE CONFIRM ORDER SALES PERSON CODE                               
*                                                                               
SVCFSP   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
         MVC   KEY,RDARKEY                                                      
         DROP  R6                                                               
         MVI   KEY,X'51'                                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(24),KEY     FIRST KEY FOUND?                             
         BNE   SVCFNO              NO  - SET CC TO NO                           
K        USING RDARREC,KEY                                                      
         CLI   K.RDARKRT,X'10'       HEADER?                                    
         BNE   SVCFNO                                                           
         DROP  K                                                                
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                  YES - RETRIEVE RECORD                        
         MVI   ELCODE,X'0A'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SVCFNO                                                           
*                                                                               
         USING RDARPPEL,R6                                                      
         MVC   SVSPCODE,RDARPPSP                                                
         DROP  R6                                                               
*                                                                               
SVCFYES  SR    RC,RC                                                            
SVCFNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*&&DO                                                                           
* VARIOUS:                                                                      
*                                                                               
VARIOUS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYHDRD,R4                                                      
*                                                                               
         XC    KEY,KEY                                                          
KYD      USING RDARKEY,KEY                                                      
         MVC   KEY(RDARKORD-RDARKEY),IOAREA                                     
         GOTO1 VHEXIN,DMCB,PAHDVARN,KYD.RDARKORD,8,0                            
         MVI   KYD.RDARKRT,X'35'                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
VAR10    DS    0H                                                               
         CLC   KEYSAVE(RDARKSEQ-RDARKEY),KEY     KEY FOUND?                     
         BNE   VARX                EXIT IF NOT                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,        X        
               AIO2,IOWORK                                                      
*                                                                               
IOD      USING RDARREC,IOAREA                                                   
         L     R1,AIO2                                                          
IO2D     USING RDARREC,R1                                                       
         CLC   IO2D.RDARPDC1,IOD.RDARPRD1                                       
         BNE   VAR20               FIND BRAND SUB-RECORD                        
         CLC   IO2D.RDARPDC2,IOD.RDARPRD2                                       
         BE    VAR40                                                            
*                                                                               
VAR20    DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',DMRSEQ),=C'REPDIR',KEYSAVE,KEY              
         B     VAR10                                                            
*                                                                               
VAR40    DS    0H                  MARK BRAND ORDER RECEIVED FROM AGY           
****     L     R1,AIO2             NOT NECESSARY CAUSE IT DIDN'T CHG            
         OI    IO2D.RDARPDFG,X'40'                                              
         MVC   IO2D.RDARPDON,IOD.RDARKORD                                       
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               AIO2,IOWORK                                                      
*                                                                               
VARX     DS    0H                                                               
         XIT1                                                                   
         DROP  IOD,IO2D,R4                                                      
         EJECT                                                                  
*&&                                                                             
*                                                                               
* RETRIEVE VARIOUS ORDER'S CONTRACT AND MARK AS VARIOUS                         
*                                                                               
MARKCON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING PAGYHDRD,R4                                                      
*                                                                               
         XC    KEY,KEY             CLEAR CONTRACT KEY AREA                      
KYD      USING RCONKEY,KEY                                                      
IOD      USING RDARREC,IOAREA                                                   
         MVI   KYD.RCONPTYP,X'8C'      SET KEY TYPE 8C                          
         MVC   KYD.RCONPREP,POWERCDE   INSERT REP ID                            
*                                  COMPLEMENT CONTRACT NUMBER                   
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),IOD.RDARREP#                                          
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KYD.RCONPCON+0(4),WORK+15                                        
         DROP  KYD,IOD                                                          
*                                  INSERT CONTRACT NUMBER, 9'S COMP             
*                                     USED AGAIN FOR REVERSED.                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BNE   MARKX               YES -                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,        X        
               ACONAREA,IOWORK                                                  
*                                                                               
         L     R2,ACONAREA                                                      
         LA    R2,34(R2)                                                        
*                                                                               
MARK10   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    MARKX                                                            
         CLI   0(R2),X'1D'                                                      
         BE    MARK30                                                           
         BH    MARKX                                                            
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     MARK10                                                           
*                                                                               
         USING RCONDREL,R2                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BNL   MARK30                                                           
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELTAREA(0),0(R2)                                                 
         MVI   ELTAREA+1,RCONDL2Q  USE NEW LENGTH                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'1D',ACONAREA),0,0              
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),ACONAREA,ELTAREA                  
*                                                                               
         L     R2,ACONAREA                                                      
         LA    R2,34(R2)                                                        
*                                                                               
MARK20   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    MARKX                                                            
         CLI   0(R2),X'1D'                                                      
         BE    MARK30                                                           
         BH    MARKX                                                            
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     MARK20                                                           
*                                                                               
         USING RCONDREL,R2                                                      
*                                                                               
MARK30   DS    0H                  UPDATE DELIVERY DATE TIME                    
         GOTO1 VDATCON,DMCB,(0,PAHDDATE),(2,RCONDRDD)                           
         GOTO1 VHEXIN,DMCB,PAHDTIME,RCONDRTD,4                                  
*                                                                               
* RESET APPROVE/REJECT/RECALL FLAGS                                             
*                                                                               
         NI    RCONDRFG,X'FF'-X'40'-X'20'-X'10'                                 
*                                                                               
*        TM    RCONDRF2,X'80'      VARIOUS?                                     
*        BZ    MARK40                                                           
         CLC   =C'VAR',PAHDTID     IS THIS A VARIOUS ORDER?                     
         BE    MARK40                                                           
*        CLC   PAHDREVN,SPACESX    REVISION??                                   
*        BE    MARKX               NO UPDATE NEEDED                             
         B     MARK45                                                           
*                                                                               
MARK40   DS    0H                                                               
         OI    RCONDRF2,X'80'      MARK CONTRACT AS VARIOUS                     
*                                                                               
MARK45   DS    0H                                                               
         CLC   PAHDREVN,SPACESX    REVISION??                                   
         BE    MARK50                                                           
         PACK  DUB(8),PAHDREVN(3)                                               
*                                  CONVERT AND STORE REVISION NUMBER            
*                                  ONE CHARACTER SHOULD BE SUFFICIENT           
         CVB   RF,DUB                                                           
         STC   RF,RCONDRRV                                                      
*                                                                               
         NI    RCONDRF2,X'FF'-X'02' RESET REVISION CANCELLED                    
*                                                                               
MARK50   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               ACONAREA,IOWORK                                                  
*                                                                               
MARKX    DS    0H                                                               
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*   CHKDAREC:  CHECKS FOR TYPE OF DARE RECORD.  SETS SOFT DELETE                
*        BIT, IF NO DELETE BIT PREVIOUSLY SET.  SETS HARD DELETE                
*        BIT, IF SOFT PREVIOUSLY SET.                                           
*                                                                               
* IF XML ORDER, SET HARD DELETE ON THE BUY RECORDS                              
*                                                                               
CHKDAREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,IOAREA                                                        
         USING RDARREC,R2          SET OUTPUT RECORD AREA                       
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   CDAR0040            NO                                           
         LA    R3,RDARELEM         YES -                                        
         USING RDARELEM,R3                                                      
*                                                                               
         TM    RDARDELS,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0020            NO  - SET IT                                 
         OI    RDARDELS,X'40'      YES - SET HARD DELETE                        
         B     CDAR0800            FINISHED                                     
CDAR0020 EQU   *                                                                
         OI    RDARDELS,X'80'      SET SOFT DELETE                              
         B     CDAR0800            FINISHED                                     
         DROP  R3                                                               
CDAR0040 EQU   *                                                                
         CLI   RDARKRT,X'40'       BUY?                                         
         BNE   CDAR0080            NO                                           
         CLI   RDARKSRT,X'00'      BUY HEADER?                                  
         BNE   CDAR0120            NO  - ORB/COMMT/DETAIL                       
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARBYEL,R3                                                      
*                                                                               
         ZIC   RF,RDARBYLN         CHECK LENGTH                                 
         LA    RE,RDARBYOL         SET OLD ELEMENT LENGTH                       
         CR    RF,RE               COMPARE OLD VS NEW                           
         BNE   CDAR0050            NOT EQUAL:  NEW LENGTH FOUND                 
         LA    RF,1(RF)            EQUAL: OLD LENGTH FOUND -                    
*                                     MUST BE INCREASED TO NEW LENGTH           
*                                     BEFORE REWRITTEN                          
         STC   RF,RDARBYLN                                                      
         MVI   RDARBYDL,X'80'      SET SOFT DELETE                              
         TM    DARFLAGS,DFXML      XML ORDER?                                   
         BZ    *+8                                                              
         OI    RDARBYDL,X'40'      YES - SET HARD DELETE                        
         ZICM  RF,RDARLEN,2                                                     
         LA    RF,1(RF)            INCREASE REC LENGTH BY 1                     
         STCM  RF,3,RDARLEN        RETURN LENGTH TO RECORD                      
         B     CDAR0800            FINISHED                                     
CDAR0050 EQU   *                                                                
         TM    RDARBYDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0060            NO  - SET IT                                 
         OI    RDARBYDL,X'40'      YES - SET HARD DELETE                        
         B     CDAR0800            FINISHED                                     
CDAR0060 EQU   *                                                                
         OI    RDARBYDL,X'80'      SET SOFT DELETE                              
         TM    DARFLAGS,DFXML      XML ORDER?                                   
         BZ    *+8                                                              
         OI    RDARBYDL,X'40'      YES - SET HARD DELETE                        
         B     CDAR0800            FINISHED                                     
         DROP  R3                                                               
CDAR0080 EQU   *                                                                
         BH    CDAR0800            SKIP OVER RECORD TYPES                       
*                                     50 (TRAILER) + 60 (EQUIVS)                
CDAR0120 EQU   *                                                                
*                                                                               
*   REMAINING RECORD TYPES ARE 15, 20, 30, 40/10, 40/20, 40/30, AND             
*        SOFT/HARD DELETE BYTE IS IN SAME PLACE IN ALL ELEMENTS.                
*        STANDARD COMMENT ELEMENT FORMAT USED, ARBITRARILY.                     
*                                                                               
         LA    R3,RDARELEM                                                      
         USING RDARELE2,R3                                                      
*                                                                               
         TM    RDARELDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0160            NO  - SET IT                                 
         OI    RDARELDL,X'40'      YES - SET HARD DELETE                        
         B     CDAR0800            FINISHED                                     
CDAR0160 EQU   *                                                                
         OI    RDARELDL,X'80'      SET SOFT DELETE                              
*                                                                               
         CLI   RDARKRT,X'40'       BUY?                                         
         BNE   CDAR0800            NO                                           
         TM    DARFLAGS,DFXML      XML ORDER?                                   
         BZ    CDAR0800                                                         
         OI    RDARBYDL,X'40'      YES - SET HARD DELETE                        
*                                                                               
         B     CDAR0800            FINISHED                                     
         DROP  R3                                                               
CDAR0800 EQU   *                                                                
         XMOD1                                                                  
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  CTRLSET:  SETS REP SE # BY LOOKING IN CONTROL FILE.                          
*                                                                               
*     IF SAME ID NUMBER COMING IN, DON'T LOOK IT UP AGAIN.                      
*        NEED TO SAVE/TEST LAST.                                                
*                                                                               
CTRLSET  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,0(R1)            SET A(TABLE INPUT)                           
         MVI   DMCB,X'0A'          SWITCH TO THE CONTROL SYSTEM                 
         BAS   RE,SWTCHSYS                                                      
         BE    CTRL0110                                                         
         MVI   RETCODE,1                                                        
         B     CTRLNO              IF NOT SUCCESSFUL, TRY AGAIN LATER           
***      BNE   EXITPRG             IF NOT SUCCESSFUL, TRY AGAIN LATER           
***                                   ALREADY THERE                             
******   GOTO1 VSWITCH,DMCB,(X'0A',X'FFFFFFFF'),0                               
***                                                                             
CTRL0110 L     R3,AUTL                                                          
         MVI   4(R3),X'0A'         SET UTL SE TO CTFILE                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'            FIND CONTROL FILE ID RECORD                  
**TEST** MVC   KEY+23(2),=X'0966'  INSERT ID NUMBER FOR EJOR!!!!                
         MVC   KEY+23(2),0(R2)     INSERT ID NUMBER FROM TABLE!!                
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IOAREA               
*                                  RETRIEVE CONTROL FILE RECORD                 
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         LA    R1,IOAREA                                                        
         CLC   KEY(25),0(R1)       CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                                                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0120 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0160            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0200            YES                                          
CTRL0160 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0120            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0200 EQU   *                                                                
         ST    R1,FULL             SAVE A(X'21' ELEMENT)                        
         L     R4,FULL             RESET A(X'21' ELEMENT)                       
*                                                                               
         L     RF,VSSB             IS THIS TST/FQA FACPAK?                      
         CLI   SSBDSPAC-SSBD(RF),C'T'                                           
         JE    CTRL0230            YES-SKIP NATIVE FACPAK CHECK                 
         CLI   SSBDSPAC-SSBD(RF),C'Q'                                           
         JE    CTRL0230            YES-SKIP NATIVE FACPAK CHECK                 
*                                                                               
         GOTOR VGETFACT,DMCB,(X'80',BYTE),(3(R4),F#SEADV)                       
         CLI   BYTE,C'Y'           IS SE NATIVE IN THIS FACPAK?                 
         JE    CTRL0230            YES-SWITCH TO IT                             
         MVI   RETCODE,2           NO-SET RETURN CODE AND                       
         J     CTRLNO                 RETURN WITH A 'NE'                        
*                                                                               
*                                     WITH REP UTL CODE                         
CTRL0230 GOTO1 VSWITCH,DMCB,(3(R4),X'FFFFFFFF'),0                               
         MVC   4(1,R3),3(R4)       OVERRIDE CONTROL FILE UTL                    
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    CTRL0280            YES - NOW FIND POWER CODE                    
         CLI   4(R1),2             SYSTEM NOT OPENED?                           
         BE    *+6                                                              
         DC    H'0'                OTHERWISE DEATH                              
CTRL0250 GOTO1 VSWITCH,DMCB,(X'0A',X'FFFFFFFF'),0                               
         MVI   RETCODE,1                                                        
         BRAS  RE,SVSYSID          SAVE SYSTEM ID                               
         CLI   THESYSID,1          IF TST SYSTEM OR                             
         BE    CTRL0260                                                         
         CLI   THESYSID,15         FQA SYSTEM, RETURN ERROR                     
         BNE   CTRLNO                                                           
CTRL0260 MVC   ERRCODE,=H'309'     REP SYSTEM NOT UP                            
         B     CTRLNO                                                           
*                                                                               
CTRL0280 EQU   *                                                                
         L     R1,VSELIST                                                       
         ZICM  RE,0(R1),2          SER UP FOR BXLE TABLE                        
         ICM   RF,15,2(R1)                                                      
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
CTRL0285 CLC   SESYS,3(R4)                                                      
         BNE   CTRL0290                                                         
         TM    SEIND,SEIRONLY+SEISETRO   READ-ONLY?                             
         BNZ   CTRL0250                                                         
         B     CTRL0295                                                         
         DROP  R1                                                               
*                                                                               
CTRL0290 BXLE  R1,RE,CTRL0285      CHECK NEXT ENTRY IN SELIST                   
*                                                                               
CTRL0295 LA    R1,IOAREA           RESET A(CONTROL RECORD)                      
         LA    R1,28(R1)           FIND X'06' AGENCY ID ELEMENT                 
CTRL0320 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - NOT FOUND????!!!                       
         CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BE    CTRL0360            YES                                          
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     CTRL0320            GO BACK FOR NEXT                             
CTRL0360 EQU   *                                                                
         MVC   POWERCDE,2(R1)      SAVE POWER CODE                              
CTRL0400 EQU   *                                                                
*                                                                               
CTRLYES  SR    RC,RC                                                            
CTRLNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SENDS AN ERROR NOTIFICATION OUT                                               
*                                                                               
* ON ENTRY:    PARAM 1             RC                                           
*              PARAM 2             ERROR NUMBER                                 
*                                  USE ERRCODE IF PARAM 2 = 0                   
***********************************************************************         
SNDERROR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   FULL,0(R1)                                                       
         OC    FULL,FULL           IF 0, USE ERRCODE                            
         BNZ   SNDE05                                                           
         MVC   FULL+2(2),ERRCODE                                                
*                                                                               
SNDE05   DS    0H                                                               
         LA    R4,R+1              LOAD A(EDICT RECORD)                         
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R6,ASPLAREA                                                      
         USING SPOOLD,R6                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPAR4     SET A(COMFACS)                               
         MVC   SPOOLBUF,ATIA       SET A(TIA)                                   
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLSUBID,=C'DAR'                                                  
*                                                                               
* CHECK IF WE ARE REALLY SENDING THE XML PARSER AN OK FLAG                      
* IF SO, CHANGE SUB ID FROM DAR TO XML                                          
         TM    DARFLAGS,DFXML                                                   
         BZ    SNDE08                                                           
         CLC   =C'XX',ERRCODE                                                   
         BNE   SNDE08                                                           
         MVC   PLSUBID,=C'XML'                                                  
*                                                                               
SNDE08   DS    0H                                                               
         MVC   PLUSER,=X'0011'    **** SJR FOR NOW                              
         MVC   PLDESC,=CL11'ERRNOT'                                             
         MVI   PLCLASS,C'G'                                                     
*        OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
         MVI   SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
*                                  AND CLEAR ALL OTHER BITS                     
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,SNDEPRNT                                                      
         B     SNDE10                                                           
*                                                                               
SNDEPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R6)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SNDE10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
         DROP  R2                                                               
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         MVC   P1+4(5),=C'*HDR*'    HEADER RECORD                               
         MVC   P1+9(14),=CL14'EDICT=*DDSDARA'                                   
         MVI   P1+34,C'W'           WIDE REPORT                                 
         MVI   P1+35,C'P'           /PAGE FOR EASYLINK AND SUPRESS TOP          
*                                                                               
         MVC   P2(14),=CL14'++DDS DAERRTRN'                                     
         BAS   RE,SNDEPRNT                                                      
*                                                                               
         LA    R2,P                                                             
         USING MDLNNOTD,R2                                                      
         MVC   MDNTTID,=C'ERRNOT'  ERROR NOTIFICATION                           
         MVC   MDNTORDR,ERRORD#    ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   MDNTFRID,ERRTOID    INSERT TO-ID IN FROM SPACE                   
         MVC   MDNTTOID,ERRFRID    INSERT FROM-ID IN TO SPACE                   
         GOTO1 VDATCON,DMCB,(5,0),(X'20',MDNTDATE)                              
*                                                                               
         GOTO1 VTICTOC,DMCB,C'SGET'       GET THE CURRENT TIME                  
         ZAP   PACKOF4B,DMCB(4)                                                 
         AP    PACKOF4B,=P'60000'  DDS TIME IS OFFSET FROM 6AM                  
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    SNDE20                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,MDNTDATE,(X'20',MDNTDATE),F'1'                       
*                                                                               
SNDE20   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,MDNTTIME,L'HALF                                
*                                                                               
         MVC   MDNTRPCN,QREPCON                                                 
         MVC   MDNTRTNS,QRETURN                                                 
         MVC   MDNTTDTE,ERRRCDT    RECEIVED DATE/TIME                           
         MVC   MDNTTTIM,ERRRCTM                                                 
*                                                                               
* CHECK IF WE ARE REALLY SENDING THE XML PARSER AN OK FLAG                      
*                                                                               
         TM    DARFLAGS,DFXML                                                   
         BZ    SNDE30                                                           
         CLC   =C'XX',ERRCODE                                                   
         BNE   SNDE30                                                           
         MVC   MDNTEFLG,=C'100'    MEANS 'OK' BACK TO ESPERANTO                 
         B     SNDE40                                                           
*                                                                               
SNDE30   DS    0H                                                               
         EDIT  (B4,FULL),(3,MDNTEFLG),FILL=0                                    
*                                                                               
SNDE40   DS    0H                                                               
         MVC   MDNTOFRI,ERRMGID    MAKEGOOD ID                                  
         MVC   MDNTSEQN,ERRVER#    MAKEGOOD VERSION #                           
*                                                                               
         BAS   RE,SNDEPRNT                                                      
*                                                                               
         MVC   P(26),=CL26'*** END OF DDS MESSAGE ***'                          
         BAS   RE,SNDEPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        YES, CLOSE PRINTQ ENTRY                      
         GOTO1 ASPOOL,DMCB,(R6)                                                 
*                                                                               
SNDEX    EQU   *                                                                
         XIT1                                                                   
         DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CREATE XML EMAIL ADDRESSES FOR NOTIFICATIONS                                  
***********************************************************************         
SETEMAIL NTR1  BASE=*,LABEL=*                                                   
         USING PAGYCOMD,R4                                                      
*                                                                               
* READ DARE RECORD INTO IOAREA                                                  
*                                                                               
         MVC   KEY,SVDARKEY                                                     
K        USING RDARKEY,KEY                                                      
         MVI   K.RDARKRT,X'10'      INSERT RECORD TYPE                          
         DROP  K                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BNE   SETEX               NO SUCH RECORD, GET OUT                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
         MVI   EMAILTYP,X'E1'                                                   
         CLI   PACMCONT+1,C'P'                                                  
         BE    *+8                 EMAIL FOR OK ORDER                           
         MVI   EMAILTYP,X'E2'                                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(EMAILTYP,IOAREA),0,0             
*                                                                               
         XC    ELTAREA(200),ELTAREA                                             
*                                  CLEAR WORKSPACE                              
         LA    R6,ELTAREA                                                       
         USING RDAREMEM,R6                                                      
         MVC   RDAREMCD,EMAILTYP   INSERT ELEMENT CODE                          
*                                                                               
         LA    R1,77                                                            
         LA    R5,PACMCONT+2                                                    
SETE23   CLI   0(R5),C' '                                                       
         BE    SETE25                                                           
         LA    R5,1(R5)                                                         
         BCT   R1,SETE23                                                        
         LA    R1,77                                                            
         B     SETE27                                                           
*                                                                               
SETE25   DS    0H                                                               
         LA    R1,PACMCONT+2                                                    
         SR    R5,R1                                                            
         LR    R1,R5                                                            
         LTR   R1,R1                                                            
         BZ    SETEX               NO EMAIL! EXIT                               
*                                                                               
SETE27   DS    0H                                                               
         AHI   R1,2                OVERHEAD                                     
         STC   R1,RDAREMLN         INSERT ELEMENT LENGTH                        
         SHI   R1,3                SUBTRACT 1 FOR EX + OVERHEAD                 
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDAREMIL(0),PACMCONT+2                                           
*                                                                               
         CLI   PACMCONT+1,C'P'                                                  
         BNE   SETE30                                                           
         MVC   OKEMAIL,RDAREMIL    EMAIL FOR OK ORDER                           
         B     SETE40                                                           
*                                                                               
SETE30   DS    0H                                                               
         CLI   PACMCONT+1,C'E'                                                  
         BNE   SETE40                                                           
         MVC   ERREMAIL,RDAREMIL                                                
*                                                                               
SETE40   DS    0H                                                               
         BAS   RE,LOADELT          LOAD ELEMENT TO RECORD                       
*                                                                               
*        GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
SETEX    EQU   *                                                                
         XIT1                                                                   
         DROP  R4,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* SENDS AN OK EMAIL TO AGENCY THAT SENT THIS XML ORDER                          
* USES ERRNOT EVEN THOUGH THE ORDER IS OK. TOO LAZY TO CREATE ANOTHER           
* TRANSACTION TYPE                                                              
***********************************************************************         
XMLEMAIL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,R+1              LOAD A(EDICT RECORD)                         
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R6,ASPLAREA                                                      
         USING SPOOLD,R6                                                        
         MVC   SPOOLID,=C'EML'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPAR4     SET A(COMFACS)                               
         MVC   SPOOLBUF,ATIA       SET A(TIA)                                   
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLSUBID,=C'EML'                                                  
         MVC   PLUSER,=X'1F9D'     DDSEMAIL                                     
         MVC   PLDESC,=CL11'XML EMAIL'                                          
         MVI   PLCLASS,C'G'                                                     
*        OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
         MVI   SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
*                                  AND CLEAR ALL OTHER BITS                     
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,XMLEPRNT                                                      
         B     XMLE10                                                           
*                                                                               
XMLEPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R6)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
XMLE10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
         DROP  R2                                                               
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         MVC   P1+4(5),=C'*HDR*'    HEADER RECORD                               
         MVI   P1+69,C'M'                                                       
         BAS   RE,XMLEPRNT                                                      
*                                                                               
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      REP                                          
         MVC   EDIPROG,=C'EML'     E-MAIL                                       
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
         BAS   RE,XMLEPRNT                                                      
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDIIDEN,=C'RCP'     PRIMARY RECIPIENT                            
         MVC   EDICOMN,OKEMAIL                                                  
         CLI   EMAILFLG,EMFOK                                                   
         BE    XMLE30                                                           
         MVC   EDICOMN,ERREMAIL                                                 
*                                                                               
XMLE30   DS    0H                                                               
         BAS   RE,XMLEPRNT                                                      
*                                                                               
         MVC   EDIDDSID,=C'++DDS'  BCC TO TEST EMAIL                            
         MVC   EDIIDEN,=C'BCC'                                                  
         MVC   EDICOMN(BCCTEMLQ),BCCTEML                                        
*                                                                               
         GOTOR AMILIVE             AM I IN TST OR LIVE REP SYSTEM?              
         BNE   XMLE40              I'M IN LIVE, BCC TO LIVE EMAIL               
*                                                                               
         MVC   EDICOMN+BCCLEMLQ(4),=C'LIVE'                                     
*                                                                               
XMLE40   DS    0H                                                               
         BAS   RE,XMLEPRNT                                                      
*                                                                               
*        MVC   EDIDDSID,=C'++DDS'                                               
*        MVC   EDIIDEN,=C'RPY'     REPLY                                        
*        MVC   EDICOMN(12),=C'DO NOT REPLY'                                     
*        BAS   RE,XMLEPRNT                                                      
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDIIDEN,=C'FRM'     FROM                                         
         MVC   EDICOMN(25),=C'DDSTVBXML@DONOVANDATA.COM'                        
         BAS   RE,XMLEPRNT                                                      
*                                                                               
*        MVC   EDIDDSID,=C'++DDS'                                               
*        MVC   EDIIDEN,=C'SDN'     SENDER                                       
*        MVC   EDICOMN(12),=C'DO NOT REPLY'                                     
*        BAS   RE,XMLEPRNT                                                      
*                                  SUBJECT LINE                                 
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDIIDEN,=C'SUB'                                                  
         MVC   EDICOMN(42),=C'TVB XML ORDER XXXXXXXX FOR STATION AAAA OX        
               K'                                                               
         MVC   EDICOMN+14(8),ERRORD#                                            
         MVC   EDICOMN+35(4),ORSTAT                                             
         CLI   EMAILFLG,EMFOK                                                   
         BE    XMLE50                                                           
*                                                                               
         CLI   EMAILFLG,EMFERROR                                                
         BNE   XMLE42                                                           
         MVC   EDICOMN+40(5),=C'ERROR'                                          
         B     XMLE50                                                           
*                                                                               
XMLE42   DS    0H                                                               
         CLI   EMAILFLG,EMFDUP                                                  
         BNE   XMLE43                                                           
         MVC   EDICOMN+40(9),=C'DUPLICATE'                                      
         B     XMLE50                                                           
*                                                                               
XMLE43   DS    0H                                                               
         CLI   EMAILFLG,EMFERBUY                                                
         BNE   XMLE50                                                           
         MVC   EDICOMN+40(16),=C'MAX BUY EXCEEDED'                              
*                                                                               
XMLE50   DS    0H                                                               
         BAS   RE,XMLEPRNT                                                      
*                                                                               
         MVC   P(80),=C'NOTE - PLEASE DO NOT REPLY TO THIS EMAIL MESSAGX        
               E. THIS EMAIL ADDRESS IS USED FOR'                               
         BAS   RE,XMLEPRNT                                                      
*                                                                               
         MVC   P(24),=C' OUTBOUND MESSAGES ONLY.'                               
         BAS   RE,XMLEPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        YES, CLOSE PRINTQ ENTRY                      
         GOTO1 ASPOOL,DMCB,(R6)                                                 
*                                                                               
         B     EXIT                                                             
         DROP  R5,R6                                                            
*                                                                               
BCCTEML  DC    C'DDSTVBXMLTESTNOTIFICATIONS@DONOVANDATA.COM'                    
BCCTEMLQ EQU   *-BCCTEML                                                        
BCCLEMLQ EQU   9                                                                
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* SAVE SYSTEM ID                                                                
***********************************************************************         
SVSYSID  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGETFACT,DMCB,0                                                  
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         MVC   THESYSID,FASYSID    SAVE THE SYSTEM ID NUMBER                    
         DROP  RE                                                               
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF I AM RUNNING IN THE LIVE SYSTEM                               
* PARM 1, BYTE 1 = X'80' ONLY RUN IN FACMEL                                     
***********************************************************************         
AMILIVE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGETFACT,DMCB,0                                                  
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         MVC   THESYSID,FASYSID    SAVE THE SYSTEM ID NUMBER                    
         DROP  RE                                                               
*                                                                               
         L     RF,VSSB             CHECK THE SYSTEM TYPE                        
         L     RF,SSBAFID-SSBD(RF)                                              
         USING FACITABD,RF                                                      
IAM10    CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                THIS BETTER EXIST ON FACIDTAB                
*                                                                               
         CLC   FACIID,THESYSID                                                  
         BE    *+12                                                             
         LA    RF,L'FACITAB(RF)                                                 
         B     IAM10                                                            
*                                                                               
         TM    FACIFL,FACIREP      ARE WE ON THE REP SYSTEM?                    
         BNZ   YES                 YES                                          
         B     NO                                                               
         DROP  RF                                                               
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* SENDS AUTO MKGROK FOR SELF-APPLIED OFFERS                                     
***********************************************************************         
DOMKGROK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R4,ASPLAREA                                                      
         USING SPOOLD,R4                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPAR4     SET A(COMFACS)                               
         MVC   SPOOLBUF,ATIA       SET A(TIA)                                   
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLSUBID,=C'DMG'                                                  
*                                                                               
         MVC   PLUSER,=X'0011'    DEFAULT TO SJR                                
         OC    SVSSID,SVSSID                                                    
         BZ    *+10                                                             
         MVC   PLUSER,SVSSID       MOVE IN THE SENDING ID                       
*                                                                               
         MVC   PLDESC(8),=C'SELFAPPL'                                           
         MVI   PLCLASS,C'G'                                                     
         MVI   SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
*                                  AND CLEAR ALL OTHER BITS                     
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,MSNCPRNT                                                      
         B     DOMOK10                                                          
*                                                                               
MSNCPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R4)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
DOMOK10  MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
         DROP  R2                                                               
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         MVC   P1+4(5),=C'*HDR*'    HEADER RECORD                               
         MVC   P1+9(14),=CL14'EDICT=*DDSDARR'                                   
         MVI   P1+34,C'W'           WIDE REPORT                                 
         BAS   RE,MSNCPRNT                                                      
*                                                                               
* PRINT A ++DDS CARD                                                            
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'                                                     
         MVC   EDIPROG,=C'MKG'     TYPE=CONFIRM                                 
         MVC   EDIIDEN,=C'TRN'                                                  
*                                                                               
         BAS   RE,MSNCPRNT                                                      
*                                                                               
* MKGROK LINE                                                                   
*                                                                               
         LA    R6,R+1              SET INPUT RECORD DSECT                       
         USING MOFRAPPD,R6                                                      
*                                                                               
         LA    R5,P                                                             
         USING MOFRCFMD,R5                                                      
         MVC   MOCFTID,=C'MKGROK'                                               
         MVC   MOCFORDR,MOAPORDR                                                
         MVC   MOCFFRID,MOAPTOID                                                
         MVC   MOCFTOID,MOAPFRID                                                
         MVC   MOCFROUT,MOAPROUT                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(5,WORK),(X'20',MOCFDATE)                           
*                                                                               
         ZAP   WORK(4),=P'0'                                                    
         THMS  DDSTIME=YES                                                      
         ST    R0,WORK             ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB              DDS TIME                                     
         AP    WORK(4),DUB(4)                                                   
*                                                                               
         CP    WORK(4),=P'240000'  PAST MIDNIGHT?                               
         BL    DOMOK20                                                          
         SP    WORK(4),=P'240000'  YES, BUMP TO NEXT DAY AND ADJUST             
         GOTO1 VADDAY,DMCB,MOCFDATE,(X'20',MOCFDATE),F'1'                       
*                                                                               
DOMOK20  DS    0H                                                               
         ICM   R1,15,WORK                                                       
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,MOCFTIME,2,0                                   
*                                                                               
         MVC   MOCFQSTA,MOAPQSTA                                                
         MVC   MOCFRPCN,MOAPRPCN                                                
         MVC   MOCFRTNS,MOAPRTNS                                                
         MVC   MOCFOFRI,MOAPOFRI                                                
         MVC   MOCFSEQN,MOAPSEQN                                                
*                                                                               
         BAS   RE,MSNCPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        YES, CLOSE PRINTQ ENTRY                      
         BAS   RE,MSNCPRNT                                                      
         DROP  R4,R5                                                            
*                                                                               
DOMOKX   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RECORD AUDIT TRAIL                                                            
*                                                                               
* ON ENTRY:    PARAM 1             BYTE 1 = ACTION                              
***********************************************************************         
AUDTRAIL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ORDAUDTR,DHNEWQ                                                  
*                                                                               
         LA    R2,IOAREA           SET OUTPUT AREA DSECT                        
         USING RDARREC,R2                                                       
*                                                                               
         MVI   RDARKRT,X'70'       SET RECORD TYPE TO 'AUDIT TRAIL'             
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SUBKEY VALUES                          
*                                  RESTORE THE KEY/OVERWRITE RECORD             
         MVC   KEY,IOAREA          SET UP KEY AGAIN                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   AUDT10              YES                                          
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,IOAREA, X        
               IOWORK                                                           
         MVI   ORDAUDTR,DHRESENQ   ORDER IS RESENT                              
         B     AUDT20                                                           
*                                                                               
* CHECK IF ORDER AUDIT TRAIL RECORD EXISTS UNDER DIFFERENT OFFICE. IF           
* IT DOES, WE NEED TO COPY OVER PREVIOUS AUDIT TRAIL HISTORY TO THE             
* NEW ORDER SO WE CAN CONTINUE WITH THE AUDIT TRAIL                             
*                                                                               
AUDT10   DS    0H                                                               
         OC    ORDARKEY,ORDARKEY                                                
         BZ    AUDT14                                                           
*                                                                               
         MVC   IOAREA(27),ORDARKEY                                              
         MVI   RDARKRT,X'70'       SET RECORD TYPE TO 'AUDIT TRAIL'             
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SUBKEY VALUES                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),IOAREA                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEYSAVE(27),KEY                                                  
         BNE   AUDT14                                                           
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,IOAREA, X        
               IOWORK                                                           
*                                                                               
         MVC   IOAREA(27),SVDARKEY                                              
*                                                                               
         MVI   RDARKRT,X'70'       SET RECORD TYPE TO 'AUDIT TRAIL'             
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SUBKEY VALUES                          
         MVI   RDARCNTL,0          MAKE ACTIVE                                  
         MVI   ORDAUDTR,DHRESENQ   ORDER IS RESENT                              
         B     AUDT20                                                           
*                                                                               
*                                                                               
* CHECK IF CONFIRMED DARE AUDIT TRAIL RECORD EXISTS. IF IT DOES, WE             
* NEED TO COPY OVER PREVIOUS AUDIT TRAIL HISTORY TO THE NEW REVISION            
* SO WE CAN CONTINUE WITH THE AUDIT TRAIL                                       
*                                                                               
AUDT14   DS    0H                                                               
         MVC   IOAREA(27),SVDARKEY                                              
*                                                                               
         MVI   RDARKRT,X'70'       SET RECORD TYPE TO 'AUDIT TRAIL'             
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SUBKEY VALUES                          
         MVC   KEY,IOAREA          SET UP KEY AGAIN                             
         MVI   KEY,X'51'           CHECK CONFIRMED ORDER RECORDS                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   AUDT15              YES                                          
         GOTO1 VDATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,IOAREA,     X        
               IOWORK                                                           
*                                                                               
         MVI   RDARKTYP,X'41'      SET RECORD TYPE BACK TO ACTIVE               
         B     AUDT20                                                           
******************************************************************              
* CHECK IF ORDER AUDIT TRAIL RECORD EXISTS UNDER DIFFERENT OFFICE. IF           
* IT DOES, WE NEED TO COPY OVER PREVIOUS AUDIT TRAIL HISTORY TO THE             
* NEW ORDER SO WE CAN CONTINUE WITH THE AUDIT TRAIL                             
*                                                                               
* EXACT KEY NOT FOUND                                                           
* NOW LOOK FOR THE ORDER # OF ALL OF THE OFFICES UNDER THIS AGENCY              
* (AGENCY OFFICE HAS CHANGED)                                                   
******************************************************************              
AUDT15   EQU   *                   LOOP THRU ALL OFFICES FOR THIS ORD#          
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKAOF-RDARKEY),SVDARKEY                                   
         MVI   KEY,X'51'                                                        
*                                                                               
AUDT17   EQU   *                   LOOP THRU ALL OFFICES FOR THIS ORD#          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(RDARKAOF-RDARKEY),KEY                                    
         BNE   AUDT19                                                           
*                                                                               
DKEYD    USING RDARKEY,KEY                                                      
         MVC   DKEYD.RDARKORD,RDARKORD                                          
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                                                               
         CLC   KEYSAVE(RDARKRT-RDARKEY),KEY                                     
         BE    AUDT18AA            SAME THRU ORDER NUMBER?                      
*                                                                               
AUDT18   DS    0H                                                               
         XC    KEY,KEY             SKIP TO NEXT OFFICE                          
         MVC   KEY(RDARKORD-RDARKEY),KEYSAVE                                    
         XR    R1,R1                                                            
         ICM   R1,3,DKEYD.RDARKAOF                                              
         AHI   R1,1                                                             
         STCM  R1,3,DKEYD.RDARKAOF                                              
         B     AUDT17                                                           
*                                                                               
AUDT18AA DS    0H                                                               
         MVI   DKEYD.RDARKRT,X'70' SET RECORD TYPE TO 'AUDIT TRAIL'             
         XC    DKEYD.RDARKSEQ(2),DKEYD.RDARKSEQ                                 
*                                  CLEAR SUBKEY VALUES                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   AUDT19              YES                                          
         GOTO1 VDATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,IOAREA,     X        
               IOWORK                                                           
*                                                                               
AUDT18A  DS    0H                                                               
         MVC   IOAREA(27),SVDARKEY                                              
         MVI   RDARKRT,X'70'       SET RECORD TYPE TO 'AUDIT TRAIL'             
         XC    RDARKSEQ(2),RDARKSEQ                                             
         B     AUDT20                                                           
*                                                                               
AUDT19   DS    0H                                                               
         MVC   IOAREA(27),SVDARKEY                                              
         MVI   RDARKRT,X'70'       SET RECORD TYPE TO 'AUDIT TRAIL'             
         XC    RDARKSEQ(2),RDARKSEQ                                             
         XCEFL RDARELEM,997        CLEAR RECORD LEAVING KEY                     
         MVC   RDARLEN,=AL2(34)                                                 
*                                                                               
AUDT20   DS    0H                  RECORD AUDIT TRAIL                           
         XC    ELTAREA,ELTAREA                                                  
DRATD    USING RDARHSEM,ELTAREA                                                 
         MVI   DRATD.RDARHSCD,X'50'                                             
         MVI   DRATD.RDARHSLN,RDARHL2Q                                          
         GOTO1 VDATCON,DMCB,(5,0),(2,DRATD.RDARHSDT)                            
         BAS   RE,GETTPWOS         GET CURRENT TIME IN HHMMSS                   
         MVC   DRATD.RDARHSTM,HHMMSS                                            
         MVI   DRATD.RDARHSER,X'FF'                                             
         MVC   DRATD.RDARHSAC,ORDAUDTR                                          
         MVC   DRATD.RDARHSVR,REVNUM                                            
*                                                                               
         CLC   SPCODE,SVSPCODE     COMPARE SALES PERSON CODE                    
         BE    *+10                                                             
         MVC   DRATD.RDARHSSP,SPCODE                                            
*                                                                               
         OI    DRATD.RDARHFG1,HFGSRQ   CHANGED BY SERVICE REQUEST               
                                                                                
         DROP  DRATD                                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),IOAREA,ELTAREA,          X        
               =C'ADD=END'                                                      
*                                                                               
         BAS   RE,RECWRITE         OUTPUT THE RECORD                            
*                                                                               
* CHECK TO SEE IF RESENT BUG HAS HAPPENED:                                      
*                                                                               
         NI    DARFLAGS,X'FF'-DFBUG                                             
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,GETEL                                                         
         BNE   AUDTX                                                            
         USING RDARHSEM,R6                                                      
         CLI   RDARHSAC,C'O'                                                    
         BNE   AUDTX                                                            
         BRAS  RE,NEXTEL                                                        
         BNE   AUDTX                                                            
         CLI   RDARHSAC,C'S'                                                    
         BNE   AUDTX                                                            
         BRAS  RE,NEXTEL           CHECK ONLY FIRST RESEND                      
         BE    AUDTX                                                            
         OI    DARFLAGS,DFBUG      FLAG IT!                                     
*                                                                               
AUDTX    DS    0H                                                               
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* UPDATE RIS A001/02 KEYS AFTER RECWRITE ROUTINE                                
* AIO2 HAS ORIGINAL OFFER HEADER RECORD                                         
* IOAREA HAS UPDATED OFFER HEADER RECORD                                        
* KEY HAS UPDATED OFFER HEADER KEY                                              
*                                                                               
UPDTRIS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* PROCESS X'A001' PASSIVE KEY                                                   
*                                                                               
         MVC   MGHDRDA,KEY+28        SAVE HEADER D/A                            
         L     R6,AIO2                                                          
         USING RMKGREC,R6                                                       
OLDMGKYD USING RMKGREC,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   OLDMGKYD.RMGSPTYP,=X'A001'                                       
         MVC   OLDMGKYD.RMGSPREP,RMKGKREP                                       
         MVC   OLDMGKYD.RMGSPSTA,RMKGKSTA                                       
         PACK  OLDMGKYD.RMGSPCON(1),RMKGKCON+3(1)   CONTRACT                    
         PACK  OLDMGKYD.RMGSPCON+1(1),RMKGKCON+2(1) REVERSE DIGITS              
         PACK  OLDMGKYD.RMGSPCON+2(1),RMKGKCON+1(1) TO GET 9'S                  
         PACK  OLDMGKYD.RMGSPCON+3(1),RMKGKCON(1)   COMPLEMENT                  
         MVC   OLDMGKYD.RMGSPGRP,RMKGKGRP                                       
         MVC   OLDMGKYD.RMGSPDAT,RMKGFOFD                                       
         MVC   OLDMGKYD.RMGSPWIP,RMKGSFG2                                       
         MVC   OLDMGKYD.RMGSPSTT,RMKGSCST                                       
*                                                                               
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    OLDMGKYD.RMGSPSTT,RMKGSLFQ SET INDICATOR                         
*                                                                               
         MVC   OLDMGKYD.RMGSPDST,RMKGSFG1                                       
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'0A'        CONTRACT INFO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   URISX               SHOULD NEVER HAPPEN...JUST EXIT              
         USING RMKGXEL,R6                                                       
         MVC   OLDMGKYD.RMGSPSAL,RMKGXSAL                                       
         MVC   OLDMGKYD.RMGSPADV,RMKGXADV                                       
         DROP  R6,OLDMGKYD                                                      
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   URIS10              NO, SKIP TO ADD NEW PASSIVE                  
*                                                                               
         OI    KEY+27,X'80'        YES, SET PURGE BIT IN KEY                    
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEY,KEY                 
*                                  REWRITE PURGED KEY                           
*                                                                               
* ADD UPDATED RIS KEY                                                           
*                                                                               
URIS10   DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         LA    R6,IOAREA           POINT TO UPDATED OFFER HEADER RECORD         
         USING RMKGREC,R6                                                       
NEWMGKYD USING RMKGREC,KEY                                                      
         MVC   NEWMGKYD.RMGSPDST,RMKGSFG1                                       
         DROP  R6,NEWMGKYD                                                      
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   URIS20              NO, SKIP TO ADD NEW PASSIVE                  
*                                                                               
         NI    KEY+27,X'FF'-X'80'  YES, RESTORE KEY                             
         MVC   KEY+28(4),MGHDRDA                                                
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEY,KEY                 
         B     URIS30                                                           
*                                                                               
URIS20   DS    0H                  ADD NEW PASSIVE KEY                          
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),MGHDRDA                                                
         GOTO1 VDATAMGR,DMCB,(X'88',DMADD),=C'REPDIR',KEY,KEY                   
*                                                                               
* PROCESS X'A002' PASSIVE KEY                                                   
*                                                                               
URIS30   DS    0H                                                               
         L     R6,AIO2                                                          
         USING RMKGREC,R6                                                       
OLDMGKYD USING RMKGREC,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   OLDMGKYD.RMGDSTYP,=X'A002'                                       
         MVC   OLDMGKYD.RMGDSREP,RMKGKREP                                       
         MVC   OLDMGKYD.RMGDSSTA,RMKGKSTA                                       
         PACK  OLDMGKYD.RMGDSCON(1),RMKGKCON+3(1)   CONTRACT                    
         PACK  OLDMGKYD.RMGDSCON+1(1),RMKGKCON+2(1) REVERSE DIGITS              
         PACK  OLDMGKYD.RMGDSCON+2(1),RMKGKCON+1(1) TO GET 9'S                  
         PACK  OLDMGKYD.RMGDSCON+3(1),RMKGKCON(1)   COMPLEMENT                  
         MVC   OLDMGKYD.RMGDSGRP,RMKGKGRP                                       
         MVC   OLDMGKYD.RMGDSDAT,RMKGFOFD                                       
         MVC   OLDMGKYD.RMGDSWIP,RMKGSFG2                                       
         MVC   OLDMGKYD.RMGDSSTT,RMKGSCST                                       
*                                                                               
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    OLDMGKYD.RMGDSSTT,RMKGSLFQ SET INDICATOR                         
*                                                                               
         MVC   OLDMGKYD.RMGDSDST,RMKGSFG1                                       
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'0A'        CONTRACT INFO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   URISX               SHOULD NEVER HAPPEN...JUST EXIT              
         USING RMKGXEL,R6                                                       
         MVC   OLDMGKYD.RMGDSDSL,RMKGXDSP                                       
         MVC   OLDMGKYD.RMGDSADV,RMKGXADV                                       
         DROP  R6,OLDMGKYD                                                      
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   URIS40              NO, SKIP TO ADD NEW PASSIVE                  
*                                                                               
         OI    KEY+27,X'80'        YES, SET PURGE BIT IN KEY                    
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEY,KEY                 
*                                  REWRITE PURGED KEY                           
*                                                                               
* ADD UPDATED RIS KEY                                                           
*                                                                               
URIS40   DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
NEWMGKYD USING RMKGREC,KEY                                                      
         MVC   NEWMGKYD.RMGDSDST,RMKGSFG1                                       
         DROP  R6,NEWMGKYD                                                      
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   URIS50              NO, SKIP TO ADD NEW PASSIVE                  
*                                                                               
         NI    KEY+27,X'FF'-X'80'  YES, RESTORE KEY                             
         MVC   KEY+28(4),MGHDRDA                                                
         GOTO1 VDATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEY,KEY                 
         B     URISX                                                            
*                                                                               
URIS50   DS    0H                  ADD NEW PASSIVE KEY                          
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),MGHDRDA                                                
         GOTO1 VDATAMGR,DMCB,(X'88',DMADD),=C'REPDIR',KEY,KEY                   
*                                                                               
URISX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MARK AGENCY HEADER AS REJECTED                                                
***********************************************************************         
MARKREJ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ DARE RECORD INTO IOAREA                                                  
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING RDARKEY,R6                                                       
         MVC   KEY,SVDARKEY                                                     
         MVI   RDARKRT,X'10'       AGENCY HEADER                                
         DROP  R6                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BNE   MRKREJX             NO SUCH RECORD, GET OUT                      
                                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'         AGENCY HEADER ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   MRKREJX                                                          
         USING RDARELEM,R6                                                      
         MVI   RDARBSTS,C'R'        MARK STATUS OF ORDER AS REJECTED            
         OI    RDARDELS,X'10'       SYSTEM REJECTED DUE TO >255                 
         DROP  R6                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               IOAREA,IOWORK                                                    
*                                                                               
MRKREJX  J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATION ROUTINES                                                           
* ON ENTRY: PARAM 1 = RC                                                        
*           PARAM 2 = ROUTINE EQUATE                                            
*           PARAM 3 = A(CODE TO BE VALIDATED)                                   
***********************************************************************         
VALIRTNS NTR1  BASE=*,LABEL=*                                                   
         L     R4,4(R1)                                                         
*                                                                               
QVSTA    EQU   1                   VALIDATE STATION CALL LETTERS                
*                                                                               
         CLI   3(R1),QVSTA                                                      
         BE    VSTA                                                             
*                                                                               
VALIYES  SR    RC,RC                                                            
VALINO   LTR   RC,RC                                                            
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
VSTA     DS    0H                                                               
         MVC   ORSTAT,0(R4)                                                     
*&&DO                                                                           
         CLI   ORSTAT+4,C'T'                                                    
         BE    VSTA20                                                           
         CLI   ORSTAT+4,C'L'       RADIO USES UID                               
         BE    VSTA20              INSTEAD OF STATION CALLS                     
*                                                                               
* NEED TO FIND STATION CALL LETTERS FOR THIS UID                                
*                                                                               
VSTA10   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTUKEY,R6                                                       
         MVI   RSTUKTYP,X'83'                                                   
         MVI   RSTUKSTP,X'08'                                                   
         MVC   RSTUKREP,POWERCDE                                                
         MVC   RSTUKUID,ORSTAT                                                  
*                                                                               
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(RSTUKSTA-RSTUKEY),KEY                                    
         BNE   VSTA30              YES                                          
*                                                                               
         MVC   ORSTAT,SPACESX                                                   
         MVC   ORSTAT(5),RSTUKSTA                                               
         MVC   0(L'PAHDQSTA,R4),ORSTAT                                          
         DROP  R6                                                               
*&&                                                                             
*                                                                               
VSTA20   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAKEY,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,POWERCDE   INSERT POWER CODE                            
         MVC   RSTAKSTA,ORSTAT     INSERT STATION                               
         CLI   RSTAKSTA+4,C'T'     TV BAND IS BLANK                             
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
         MVI   RSTAKSTA+5,C' '     CLEAR LAST CHAR OF STATION FIELD             
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 VDATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND? (ENTIRE KEY CHECKED)              
         BE    VALIYES             YES                                          
*                                                                               
VSTA30   DS    0H                                                               
         MVC   ERRCODE,=H'6'       SET ERROR, STATION NOT FOUND                 
         B     VALINO                                                           
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE FAPQPL                                                         
         EJECT                                                                  
       ++INCLUDE REPFACSQ                                                       
         EJECT                                                                  
       ++INCLUDE CTGENRAD                                                       
         EJECT                                                                  
       ++INCLUDE REGENSCN                                                       
         EJECT                                                                  
*                                                                               
WORKD    DSECT                                                                  
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
         ORG   DMCB                                                             
PAR1     DS    F                                                                
PAR2     DS    F                                                                
PAR3     DS    F                                                                
PAR4     DS    F                                                                
PAR5     DS    F                                                                
PAR6     DS    F                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SAVER1   DS    A                                                                
ATIA     DS    A                                                                
AUTL     DS    A                                                                
FULL     DS    F                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
SVDARKEY DS    CL32                                                             
ORDARKEY DS    CL32                ORGINAL DARE KEY BEFORE OFF. CHANGE          
SVCPYKEY DS    CL32                                                             
HALF     DS    H                                                                
BYTE     DS    C                                                                
UPDATSW  DS    C                   REWRITE NEEDED FLAG                          
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VGETFACT DS    A                                                                
VSWITCH  DS    A                                                                
VHELLO   DS    A                                                                
VDATCON  DS    A                                                                
VDATVAL  DS    A                                                                
VCALLOVL DS    A                                                                
VLOCKET  DS    A                                                                
VGETDAY  DS    A                                                                
VADDAY   DS    A                                                                
ASPLAREA DS    A                                                                
ACONAREA DS    A                                                                
AIO2     DS    A                                                                
APQREC   DS    A                                                                
ASPOOL   DS    A                                                                
APASAREA DS    A                   PASSIVE POINTER WORK AREA                    
VCOMFACS DS    A                                                                
VREPFACS DS    A                                                                
WORK     DS    CL64                                                             
DATADISP DS    H                                                                
ELCODE   DS    XL1                                                              
PQINDEX  DS    CL40                PRINT QUEUE INDEX                            
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL210               PQ RECORD DATA                               
LASTAGY  DS    CL2                 LAST AGENCY/REP NUMBER                       
POWERCDE DS    CL2                 POWER CODE TO USE                            
LASTREC  DS    XL1                 LAST RECORD INDICATOR                        
INPROG   DS    XL1                 CURRENT RECORD INDICATOR                     
RESENTFL DS    CL1                 RESENT INDICATOR:                            
*                                  Y  =  YES ORDER HAS BEEN RESENT              
DARFLAGS DS    X                   DARE FLAGS                                   
DFOFFCHG EQU   X'80'               X'80' = AGENCY ROUTING OFF CHANGED           
DFCONCF  EQU   X'40'               X'40' = CONTRACT IS CONFIRM NOW              
DFEDI    EQU   X'20'               X'20' = CONTRACT IS KATZ/EDI DARE            
DFMGCHK  EQU   X'10'               X'10' = SPECIAL MG/ALL CHECK                 
DFBUG    EQU   X'01'               X'01' = RADIO PASSIVE KEY BUG FOUND!         
DFCEDATE EQU   X'02'               X'02' = CONFLICT END DATE                    
DFXML    EQU   X'04'               X'04' = XML ORDER                            
DFINLOOP EQU   X'08'               X'08' = CALL SRRDR00 AGAIN IF MORE           
*                                          DAR ENTRIES ARE FOUND                
*                                                                               
HDRDA    DS    XL4                                                              
SAVECON# DS    XL4                 SAVE AREA FOR CONTRACT NUMBER                
SAVEMISC DS    CL1                 SAVE AREA FOR RDARMISC                       
PREVNUM  DS    X                   PREVIOUS REVISION NUMBER                     
REVNUM   DS    X                   CURRENT REVISION NUMBER                      
ELTAREA  DS    CL200               ELEMENT BUILD AREA                           
WORKTABL DS    8CL7                TEMPORARY JOB TABLE                          
*                                  8 7-CHARACTER ENTRIES                        
*                                     BYTES 1 - 2  =  USER ID IN HEX            
*                                     BYTES 3 - 4  =  SUBID                     
*                                     BYTES 5 - 6  =  REPORT ID BINARY          
*                                  COVERED BY DSECT: DREPTABL                   
DUMPCTR  DS    XL1                                                              
TESTCTR  DS    XL1                                                              
DAILYFLG DS    CL1                 W  =  WEEKLY                                 
*                                  D  =  DAILY                                  
ORORD#   DS    CL8                 ORDER REJECT ORDER #                         
ORCON#   DS    CL8                 ORDER REJECT CONTRACT #                      
ORSTAT   DS    CL6                 ORDER STATION                                
ORAGY    DS    CL3                 ORDER ROUTING CODE                           
ORAGYOFF DS    CL2                                                              
OREQUIVS DS    CL20                DAR EQUIV CODES:  4X5                        
OREQUDEL DS    XL1                 DELIMITER FOR OREQUIVS                       
MGOFF    DS    CL2                 OFFICE OF MAKEGOOD ORDER                     
MGSTA    DS    CL5                 STATION OF MAKEGOOD ORDER                    
MGINPROG DS    CL1                 Y  =  MAKEGOOD REJECT IN PROGRESS            
SAVTIME  DS    F                   SAVE AREA FOR TIME                           
HHMMSS   DS    XL3                 TIME IN PWOS (HHMMSS)                        
AHIATUS  DS    F                   A(HIATUS INSERT SLOT)                        
PACKOF4B DS    PL4                 PACKED NUMBER OF 4 BYTES                     
QREPCON  DS    CL8                 COPY OF REP CONTRACT NUMBER                  
QRETURN  DS    CL16                COPY OF RETURN TO SENDER DATA                
ERRBLOCK DS    0C                                                               
ERRORD#  DS    CL8                 FROM ORD # FOR ERROR NOTIFICATION            
ERRFRID  DS    CL10                FROM ID FOR ERROR NOTIFICATION               
ERRTOID  DS    CL10                TO   ID FOR ERROR NOTIFICATION               
ERRRCDT  DS    CL6                 RECEIVED DATE FOR ERROR NOTIFICATION         
ERRRCTM  DS    CL4                 RECEIVED TIME FOR ERROR NOTIFICATION         
ERRMGID  DS    CL3                 MAKEGOOD ID FOR ERROR NOTIFICATION           
ERRVER#  DS    CL2                 MAKEGOOD VER# FOR ERROR NOTIFICATION         
ERRBLKLQ EQU   *-ERRBLOCK                                                       
ERRCODE  DS    XL2                 ERROR CODE                                   
HIASTART DS    XL2                 FLIGHT START DATE COMPRESSED                 
HIAEND   DS    XL2                 FLIGHT END   DATE COMPRESSED                 
ROTSTART DS    X                   BUY ROTATION START DAY                       
GROSPOTS DS    F                   GROSS SPOTS                                  
GROSDOLS DS    F                   GROSS DOLLARS                                
MGAUDELT DS    CL32                MAKEGOOD AUDIT TRAIL ELEMENT AREA            
ORDAUDTR DS    X                   ORDER AUDIT TRAIL ACTION                     
IOWORK   DS    12D                 IO WORK AREA                                 
*                                                                               
OLDSTA   DS    0XL3                OLD DARE ORDER STATUS GROUP                  
OLDBSTS  DS    X                                                                
OLDMEDI  DS    C                   OLD MEDIA TYPE                               
OLDFLG1  DS    X                   OLD RDARFLG1                                 
*                                                                               
RETCODE  DS    X                   RETURN CODE TO THE CALLER                    
*                                                                               
THESYSID DS    XL1                 SAVED SYSTEM ID                              
*                                                                               
VERSION  DS    XL1                                                              
SVSSID   DS    CL2                                                              
SPCODE   DS    XL3                 SAVED SALES PERSON CODE                      
SVSPCODE DS    XL3                 PREVIOUS  SALES PERSON CODE                  
AUDITYP  DS    C                   SAVED AUDIT TRAIL TYPE                       
SVBYRO   DS    XL(L'RDARBYRO+L'RDARBYRS)                                        
SVESEND  DS    XL(L'RDARESEN)      SAVED EST END DATE                           
*                                                                               
ABUYNDX  DS    X                   AGENCY BUY INDEX                             
ABUYNUM  DS    XL2                 ACTUAL XML AGENCY BUY NUMBER                 
*                                                                               
EMAILFLG DS    X                   FLAGS FOR XML EMAIL NOTIFICATIONS            
EMFOK    EQU   X'80'               ORDER OK                                     
EMFDUP   EQU   X'40'               DUPLICATE ORDER                              
EMFERBUY EQU   X'20'               NUMBER OF BUYS EXCEEDED (254)                
EMFERROR EQU   X'01'               SOM TIN WONG                                 
*                                                                               
EMAILTYP DS    X                   E1 OR E2 ELEMENT CODE                        
ERREMAIL DS    CL77                ERROR EMAIL ADDRESS FOR XML ORDERS           
OKEMAIL  DS    CL77                OK EMAIL ADDRESS FOR XML ORDERS              
*                                                                               
MQFLAGS  DS    X                                                                
MQOFFSET DS    F                                                                
AMQAREA  DS    A                                                                
MGHDRDA  DS    XL4                                                              
*                                                                               
BUYCOUNT DS    H                   BUY COUNTER                                  
BUYHIFLG DS    XL1                 BUY TOO HIGH FLAG                            
         DS    XL1                 SPARE                                        
*                                                                               
IOAREA   DS    6000C                                                            
IOAREA2  DS    6000C                                                            
*                                                                               
PASAREA  DS    CL1600                                                           
SPULAREA DS    XL3200              SPOOL AREA                                   
MQAREA   DS    XL3200              MQ BUFFER AREA                               
CONAREA  DS    6000C               CONTRACT REC AREA                            
CONLENQ  EQU   6000                                                             
         DS    0D                                                               
PQREC    DS    14336X              PRINT QUEUE BUFFER                           
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
AGCWORKD DSECT                                                                  
AGCANDA  DS    XL4                                                              
AGCWORKQ EQU   *-AGCWORKD                                                       
*                                                                               
*   DREPTABL:  DSECT COVERING THE SRTIM-GENERATED TABLE ENTRIES                 
*                                                                               
DREPTABL DSECT                                                                  
*                                                                               
DRUSERID DS    XL2                 BINARY USER ID CODE                          
DRSUBID  DS    CL3                 SUB ID                                       
DRREPT#  DS    XL2                 BINARY REPORT NUMBER                         
LCORTAB  EQU   *-DRUSERID          LENGTH OF ENTRY                              
DDSTMADJ EQU   6                                                                
*                                                                               
DBWORKD  DSECT                                                                  
DBWORK   DS    12D                                                              
DBELEM   DS    XL255                                                            
DBIO     DS    XL3650                                                           
DBWORKQ  EQU   *-DBWORKD                                                        
*                                                                               
*                                                                               
*   DSECT COVERING DARE RECORDS                                                 
*                                                                               
RDARSECT DSECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
*                                                                               
*   DSECT COVERING PRINT QUEUE INDEX                                            
*                                                                               
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
* DDCOMFACS                                                                     
* FAFACTS                                                                       
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
       ++INCLUDE SRRDRFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
RAGYRECD DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
         EJECT                                                                  
RBUYRECD DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
RMKGRECD DSECT                                                                  
       ++INCLUDE REGENMKG                                                       
         EJECT                                                                  
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE SPDARMKGDD                                                     
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
         EJECT                                                                  
       ++INCLUDE EDILINKD                                                       
CFCD     DSECT                                                                  
       ++INCLUDE REGENCFC                                                       
         EJECT                                                                  
RSALRECD DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE FACIDTABD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SRDAR10   06/24/19'                                      
         END                                                                    
