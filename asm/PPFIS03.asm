*          DATA SET PPFIS03    AT LEVEL 071 AS OF 03/28/07                      
*PHASE T41B03A                                                                  
         TITLE 'PPFIS03 - PRINTPAK FIS  EST TOTAL DISPLAY'                      
*                                                                               
*      CHANGE LOG                                                               
*                                                                               
*   BOBY 01/07     ADD PLANNED COSTS OPTION                                     
*                                                                               
*   BOBY 10/05     2 CHARACTER MEDIA OFFICE CODES                               
*                                                                               
*   SMYE 10/17/97  NEW LIMIT ACCESS SECURITY INCLUDING TRAFFIC OFFICE           
*                  ALSO CHANGED ALL USE OF R9 TO R2 TO MAKE R9                  
*                  AVAILABLE AS A SECOND BASE (SEE *CHG9*)                      
*                                                                               
*   KWAN 12/00     BILL AMTS ARE NOW IN PL6 INSTEAD OF PL5                      
*                                                                               
*   SMYE 10/17/97  CHANGED CLI.. TO TM.. FOR TESTING TEST ESTIMATE              
*                  FIELD IN PROCS MFLD5 AND MFLD6B                              
*                                                                               
*   SMYE 12/20/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                   
*                                                                               
*   BPLA 5/95      SKIP ESTIMATES WHOSE DATES ARE OUTSIDE OF                    
*                  REQUEST PERIOD (IF REPORTING BILLABLE MONTHS                 
*                  CONSIDER 6 MONTHS BEFORE AND 3 MONTHS AFTER                  
*                                                                               
*   BPLA 5/95      CLEAR DATASW AND SET IT FROM PREVDSW IF I HAVE               
*                  A PREVKEY                                                    
*                                                                               
*   BPLA 1/26/95   IMPROVE MAX IO CHECKING                                      
*                  IF MAXIOS REACHED IN GETNCLT GO TO STOP                      
*                  SHOULD PREVENT DUMPS IN TASKNEXT                             
*                                                                               
*                                                                               
*   BPLA 3/1/94    CHANGES FOR BILLING GROUPS                                   
*                                                                               
T41B03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41B03,R9         R9 AS SECOND BASE                            
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T41BFFD,RA                                                       
*                                                                               
         CLI   PVSW,1              SEE IF ALL HEADS PREV VAL                    
         BE    DISPLAY             HAVE ALREADY READ DATA                       
*                                  CAN GO TO DISPLAY LOGIC                      
PROCESS  LA    R2,ACCMLEN*6/8                                                   
         LA    R3,BILLG            CLEAR ACCUMS                                 
*                                                                               
PROC1    ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R2,PROC1                                                         
*                                                                               
PROC4    DS    0H                                                               
*                                                                               
         MVI   DATASW,0                                                         
*                                                                               
         XC    CPELIST,CPELIST     INIT CLT/PRD/EST LIST                        
         OC    PREVKEY,PREVKEY     SEE IF DOING CONTINUATION                    
         BNZ   PROC20              SKIP TO BUCKET READ                          
*                                                                               
*        NO NEED TO SET RMLIST                                                  
*                                                                               
PROC20   DS    0H                                                               
         EJECT                                                                  
RBUCS    DS    0H                  READ BUCKETS FOR ORDERED + PAID              
         ZAP   CTR,=P'13'          CTR FOR MAX BUCKET RECORDS                   
         OC    PREVKEY,PREVKEY     SEE IF DOING CONTINUATION                    
         BZ    RB2                                                              
         MVC   DATASW,PREVDSW      SET DATASW FROM PREVDSW                      
*                                  IF DOING A CONTINUATION                      
         MVC   KEY,PREVKEY                                                      
         MVC   WORKCLT,KEY+4       NEED TO RESET WORKCLT                        
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HIGH                                                          
         B     RBK3                NOW SEQ READ                                 
*                                                                               
RB2      MVC   WORKCLT,SVCLT                                                    
         CLI   SVCLT,C'&&'         BILLING GROUP                                
         BE    RB3                                                              
         CLI   SVCLT,C'*'          OFFICE                                       
         BE    RB3                                                              
         CLI   SVCLT,C'$'          OFFICE LIST                                  
         BE    RB3                                                              
         CLC   SVCLT,=C'ALL'                                                    
         BNE   RB5                                                              
RB3      XC    WORKCLT,WORKCLT                                                  
         BAS   RE,GETNCLT                                                       
         CLC   WORKCLT(3),=X'FFFFFF'   SEE IF MAX IOS REACHED                   
         BE    STOP                MUST STOP                                    
*                                                                               
         OC    WORKCLT,WORKCLT                                                  
         BZ    RBILLS              NO CLT FOUND                                 
*                                                                               
*        READ BUCKETS FOR CLIENT                                                
*                                                                               
RB5      XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PBKKEY,R4           ESTABLISH BUCKET RECORD KEY                  
*                                                                               
         MVC   PBKKAGY,AGYALPHA     AGENCY                                      
         MVC   PBKKMED,SVMED        MEDIA                                       
         MVI   PBKKRCD,X'09'        RECORD ID                                   
         MVC   PBKKCLT,WORKCLT      CLIENT                                      
*                                                                               
         CLC   SVPRD,=C'ALL'       SET PRODUCT IF DOING 1 PRODUCT               
         BE    *+10                                                             
         MVC   PBKKPRD,SVPRD                                                    
*                                                                               
         OC    SVESTB,SVESTB       SET ESTIMATE IF DOING 1 ESTIMATE             
         BZ    *+10                                                             
         MVC   PBKKEST,SVESTB                                                   
*                                                                               
         XC    WORKPRD,WORKPRD     INIT PRODUCT CODE SAVEAREA                   
*                                                                               
RBK2     BAS   RE,HIGH             READ FIRST BUCKET RECORD                     
         B     RBK5                                                             
*                                                                               
RBK3     BAS   RE,SEQ              READ NEXT RECORD                             
*                                                                               
RBK5     MVC   PREVKEY,KEY                                                      
*                                                                               
         CLC   KEY(PBKKPRD-PBKKEY),KEYSAVE      END OF CLT                      
         BNE   RBKX                YES - GO READ BILLS OR NEXT CLT              
*                                                                               
         GOTO1 GETFACT,DMCB,0      MAKE SURE WE AREN'T OVER MAXIO COUNT         
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLC   FATIOCNT,SVMAXIO                                                 
         BNL   STOP                                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R4,KEY              POINT TO DIRECTORY KEY                       
*                                                                               
         CLI   PBKKPRD,C'*'        SKIP AOR PRDS                                
         BE    RBK3                                                             
*                                                                               
         CLC   SVPRD,=C'ALL'       SKIP IF ALL PRODUCTS                         
         BE    RBK7                                                             
*                                                                               
         CLC   PBKKPRD,SVPRD       ELSE                                         
         BNE   RBXX                DONE AT END OF PRODUCT                       
*                                                                               
         B     RBK10               SKIP EST CHK                                 
*                                                                               
RBK7     OC    SVESTB,SVESTB       IF ESTIMATE GIVEN                            
         BZ    RBK10                                                            
*                                                                               
         CLC   PBKKEST,SVESTB         OKAY IF ESTIMATE MATCHES                  
         BE    RBK10                                                            
         BH    RBK8                   FORCE NEXT PRODUCT IF HIGH                
*                                     LOW                                       
         MVC   PBKKEST,SVESTB         BUMP TO WANTED ESTIMATE                   
         B     RBK2                   READ HIGH FOR THIS EST                    
*                                                                               
RBK8     MVC   PBKKEST,=2X'FF'        GET NEXT PRD                              
         B     RBK2                                                             
*                                                                               
RBKX     CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BE    RBX5                                                             
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    RBX5                                                             
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    RBX5                                                             
*                                                                               
         CLC   SVCLT,=C'ALL'                                                    
         BNE   RBXX                                                             
*                                                                               
RBX5     BAS   RE,GETNCLT          GET NEXT CLIENT                              
*                                                                               
         CLC   WORKCLT(3),=X'FFFFFF'   MEANS MAX IO REACHED                     
         BE    STOP                  MUST STOP                                  
*                                                                               
         OC    WORKCLT,WORKCLT     DONE IF NO MORE CLIENTS                      
         BZ    RBXX                                                             
         B     RB5                 GO PROCESS THIS CLT                          
*                                                                               
RBXX     XC    PREVKEY,PREVKEY     DONE                                         
         B     RBILLS              GO READ BILLS                                
*                                                                               
RBK10    DS    0H                  PROCESS BUCKET                               
*                                                                               
         CLC   PBKKPRD,WORKPRD     SKIP IF NOT NEW PRD                          
         BE    RBKPRDX                                                          
*                                                                               
         XC    WORKPBLD,WORKPBLD   INIT PRODUCT BILLING EFFECTIVE DATE          
*                                                                               
*        MUST READ PRDREC TO GET BILLING EFFECTIVE DATE                         
*                                                                               
         MVC   ESAVKEY,KEY         SAVE CURRENT BUCKET KEY                      
*                                                                               
*                                  DIFFERS IN CODE ONLY FROM BUCKET KEY         
*                                  AND NO ESTIMATE NUMBER                       
*                                                                               
         MVI   PBKKRCD,X'06'       SET THE RECORD CODE                          
         XC    PBKKEST,PBKKEST     ELIMINATE ESTIMATE CODE                      
*                                                                               
         BAS   RE,HIGH             READ PRODUCT  RECORD                         
*                                                                               
         CLC   PBKKEY,KEYSAVE      MUST FIND PRODUCT                            
         BNE   RBKPRDNF            PRD NOT FOUND SKIP BUCKET                    
*                                                                               
         BAS   RE,GETREC           READ IN PRODUCT  RECORD                      
*                                                                               
         MVC   KEY,ESAVKEY         RESTORE CURRENT BUCKET KEY                   
         BAS   RE,HIGH             RESTORE SEQ READ                             
*                                                                               
         B     RBKPRDFD                                                         
*                                                                               
RBKPRDNF DS    0H                  GET HERE IF PRD NOT ON FILE                  
*                                                                               
         MVC   KEY,ESAVKEY         RESTORE CURRENT BUCKET KEY                   
         BAS   RE,HIGH             RESTORE SEQ READ                             
*                                                                               
         B     RBK3                READ NEXT BUCKET                             
*                                                                               
RBKPRDFD DS    0H                                                               
*                                                                               
         MVC   WORKPRD,PBKKPRD     SAVE PRODUCT CODE                            
*                                                                               
         MVI   ELCODE,PPBPCECQ     LOOK FOR BILLING EFD ELEMENT                 
         LA    R2,PPRDELEM         SEARCH PRODUCT  RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RBKPRDBX            NONE                                         
*                                                                               
         USING PPRDBPCE,R2         ESTABLISH BILLING EFD ELEMENT                
*                                                                               
         MVC   WORKPBLD,PPBPCEFF   SAVE PRODUCT BILLING EFD                     
*                                                                               
         DROP  R2                                                               
*                                                                               
RBKPRDBX DS    0H                                                               
*                                                                               
RBKPRDX  DS    0H                                                               
*                                                                               
         LA    R8,CPELIST          POINT TO CLT/PRD/EST LIST                    
         SR    R2,R2               COUNTER FOR INTERNAL BUCKETS                 
*                                  COUNTS NUMBER OF DOUBLE WORDS                
         LA    R7,13               MAX NUMBER OF ROWS                           
*                                  12 MONTHS AND A TOTAL OR SPARE               
*                                                                               
*        FIND LAST BUCKET IN TABLE                                              
*                                                                               
         USING CPELISTD,R8         ESTABLISH ENTRY IN LIST                      
*                                                                               
RBK10C   OC    CPELISTD(CPEDONE-CPELISTD),CPELISTD  DONE AT END OF LIST         
         BZ    RBK10E                 NO CLT/PRD/EST                            
*                                                                               
         LA    R2,8(R2)            BUMP COUNTER                                 
         LA    R8,CPELISTL(R8)                                                  
         BCT   R7,RBK10C                                                        
*                                                                               
         DC    H'0'                I'VE READ TOO MANY BUCKETS                   
*                                                                               
RBK10E   DS    0H                                                               
*                                                                               
         STC   R2,CPEDONE          SAVE DISP INTO INTERNAL BUCKETS              
         ST    R2,DISP             SAVE DISP INTO INTERNAL BUCKETS              
*                                                                               
*        MUST READ EST TO GET TEST STATUS AND ACTUALIZATION DATE                
*                                                                               
         MVC   ESAVKEY,KEY         SAVE CURRENT BUCKET KEY                      
*                                                                               
         LA    R4,KEY                                                           
         USING PESTKEY,R4          SWITCH TO ESTIMATE KEY                       
*                                  DIFFERS IN CODE ONLY FROM BUCKET KEY         
*                                                                               
         MVI   PESTKRCD,X'07'      SET THE RECORD CODE                          
*                                                                               
         BAS   RE,HIGH             READ ESTIMATE RECORD                         
*                                                                               
         CLC   PESTKEY,KEYSAVE     MUST FIND EST                                
         BNE   RBK10HE             EST NOT FOUND SKIP BUCKET                    
*                                                                               
         DROP  R4                                                               
*                                                                               
         LA    R0,ESTREC                                                        
         ST    R0,AREC                                                          
*                                                                               
         BAS   RE,GETREC           READ IN ESTIMATE RECORD                      
*                                                                               
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
*                                                                               
RBK10H   MVC   KEY,ESAVKEY         RESTORE CURRENT BUCKET KEY                   
         BAS   RE,HIGH             RESTORE SEQ READ                             
*                                                                               
         B     RBK10HX                                                          
*                                                                               
RBK10HE  DS    0H                  GET HERE IF EST NOT ON FILE                  
*                                                                               
         MVC   KEY,ESAVKEY                                                      
         BAS   RE,HIGH             RESTORE SEQ READ                             
*                                                                               
         B     RBK3                AND SKIP THIS ESTIMATE                       
*                                                                               
RBK10HX  DS    0H                                                               
*                                                                               
         CLI   SUBSW,1             SEE IF DOING SUBSEQUENT MTHS                 
         BE    RBK10M              THEN DON'T CHECK ESTIMATE START              
*                                                                               
*        CHECK ESTIMATE START AND END DATES FOR INCLUSION                       
*                                                                               
         MVC   WORK(6),PESTST      START DATE 6CH                               
         MVC   WORK+6(6),PESTST    START DATE 6CH                               
*                                                                               
         CLI   MTHSW,C'I'           SEE IF DOING INSERTION DATES                
         BE    RBK10H5              THEN DON'T ADJUST                           
*                                                                               
*        USE ANY ESTIMATE 6 MONTHS OLD OR LESS                                  
*                                                                               
         MVC   WORK+4(2),=C'01'    SET START TO 1ST OF MONTH                    
         GOTO1 VADDAY,DMCB,WORK,WORK+6,-190                                     
*                          190 DAYS MUST GET ME 6 MONTHS BACK                   
*                          BILLABLE MONTH CAN BE 6 MONTHS PRIOR                 
*                                                                               
RBK10H5  XC    WORK(6),WORK                                                     
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,WORK)  BINARY FORM                    
*                                                                               
         CLC   WORK(2),SVENDB                                                   
*                            ADJUSTED ESTIMATE START IS AFTER                   
*                            END OF REQUEST PERIOD                              
         BH    RBK3                SKIP THIS ESTIMATE                           
*                                                                               
RBK10M   DS    0H                                                               
*                                                                               
         CLI   PRSW,1              SEE IF DOING PRIOR MTHS                      
         BE    RBK10X                                                           
*                                                                               
         MVC   WORK(6),PESTEND                                                  
         MVC   WORK+6(6),PESTEND                                                
*                                                                               
         CLI   MTHSW,C'I'          SEE IF DOING INSERTION DATES                 
         BE    RBK10M5                                                          
*                                                                               
*        END DATE CAN BE UP TO 3 MONTHS IN FUTURE                               
*                                                                               
         MVC   WORK+4(2),=C'01'                                                 
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+6,95                                       
*                           95 DAYS MUST GET ME 3 MONTHS AHEAD                  
*                           BILLABLE MONTH CAN BE 3 MONTHS AFTER                
RBK10M5  XC    WORK(6),WORK                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,WORK)  BINARY                         
*                                                                               
         CLC   WORK(2),SVSTRTB                                                  
*                           ADJUSTED ESTIMATE END IS                            
*                           BEFORE START OF REQUEST PERIOD                      
         BL    RBK3                SKIP THIS ESTIMATE                           
*                                                                               
RBK10X   DS    0H                                                               
*                                                                               
         MVI   DATASW,X'01'        SET DATA FOUND                               
*                                                                               
         MVC   CPELISTD(CPEDONE-CPELISTD),PESTKCLT    SAVE CLT/PRD/EST          
         MVC   CPEESTA,PESTTEST    SAVE TEST STATUS                             
*                                                                               
         MVI   ELCODE,PEACTECQ     LOOK FOR ACTUALIZATION ELEMENT               
*                                                                               
         LA    R2,PESTELEM         SEARCH ESTIMATE RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RBK10Y              NONE                                         
*                                                                               
         USING PESTACTD,R2         ESTABLISH ACTUALIZATION ELEMENT              
*                                                                               
         MVC   CPEEACTD,PEACTDAT   SAVE ACTUALIZATION DATE                      
*                                                                               
RBK10Y   DS    0H                                                               
*                                                                               
         MVI   ELCODE,PEBPCECQ     LOOK FOR BILLING EFD ELEMENT                 
         LA    R2,PESTELEM         SEARCH ESTIMATE RECORD                       
         BAS   RE,NEXTEL           FIND ELEMENT                                 
         BNE   RBK10Z              NONE                                         
*                                                                               
         USING PESTBPCE,R2         ESTABLISH BILLING EFD ELEMENT                
*                                                                               
         MVC   CPEEBEFD,PEBPCEFF   SAVE BILLING EFD                             
*                                                                               
         DROP  R2                                                               
*                                                                               
RBK10Z   DS    0H                                                               
*                                                                               
         OC    CPEEBEFD,CPEEBEFD   IF NO ESTIMATE BILLING DATE                  
         BNZ   *+10                                                             
         MVC   CPEEBEFD,WORKPBLD      USE PRODUCT BILLING EFD                   
*                                                                               
         OC    CPEEBEFD,CPEEBEFD   IF NO PRODUCT  BILLING DATE                  
         BNZ   *+10                                                             
         MVC   CPEEBEFD,WORKCBLD      USE CLIENT  BILLING EFD                   
*                                                                               
RBK11    DS    0H                                                               
*                                                                               
         BAS   RE,GETREC           READ IN BUCKET RECORD                        
*                                                                               
         L     R4,AREC             ESTABLISH BUCKET RECORD                      
         USING PBKREC,R4                                                        
*                                                                               
         MVI   ELCODE,X'22'        FIND REGULAR BUCKETS                         
         LA    R2,BKELEM                                                        
         USING BKELEM,R2                                                        
*                                                                               
         CLI   0(R2),X'22'                                                      
         BE    RBK15                                                            
*                                                                               
RBK12    BAS   RE,NEXTEL                                                        
*                                                                               
         BNE   RBK25               NO MORE ELEMENTS                             
*                                                                               
RBK15    LA    R3,BKYM                                                          
*                                                                               
         BAS   RE,FINDISP                                                       
         BNE   RBK12                                                            
*                                                                               
RBK20    L     R6,DISP             DISPLACEMENT INTO INTERNAL BUCKETS           
*                                                                               
*        UPDATE PAID BUCKETS                                                    
*                                                                               
         LA    R5,BILLG            START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) PAID BUCKET TO BE UPDATED                    
         AP    0(8,R5),BKPGRS      UPDATE PAID GROSS                            
*                                                                               
         LA    R5,BILLN            START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) BUCKET TO BE UPDATED                         
         AP    0(8,R5),BKPNET      UPDATE PAID NET                              
*                                                                               
         LA    R5,BILLCD           START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) BUCKET TO BE UPDATED                         
         AP    0(8,R5),BKPCD       UPDATE PAID CD                               
*                                                                               
*        UPDATE ORDERED BUCKETS                                                 
*                                                                               
         CLI   PLNCSTSW,C'P'       IF DOING PLANNED COSTS                       
         BNE   RBK23                  SKIP ORDERED $'S IN SOME CASES            
*                                                                               
         CLI   WORKPCYN,C'Y'       IF NOT DOING PC BILLING                      
         BNE   RBK23                  USE ORDERED $'S                           
*                                                                               
         OC    CPEEBEFD,CPEEBEFD   IF NO BILLING EFFECTIVE DATE                 
         BZ    RBK23                  USE ORDERED DOLLARS                       
*                                                                               
         CLC   BKYM,CPEEBEFD       IF BUCKET MONTH IS BEFORE BEFD               
         BL    RBK23                  USE ORDERED DOLLARS                       
*                                                                               
         OC    CPEEACTD,CPEEACTD   IF NO ACTUALIZATION DATE                     
         BZ    RBK24                  SKIP ORDERED DOLLARS                      
*                                                                               
         CLC   BKYM,CPEEACTD       IF AFTER ACTUALIZATION DATE                  
         BH    RBK24                  SKIP ORDERED DOLLARS                      
*                                                                               
RBK23    DS    0H                                                               
*                                                                               
         LA    R5,BILLG            START OF BUCKETS                             
         LA    R5,0(R6,R5)         BUCKET TO UPDATE                             
         AP    0(8,R5),BKOGRS      UPDATE GROSS ORDERED                         
*                                                                               
         LA    R5,BILLN                                                         
         LA    R5,0(R6,R5)         BUCKET TO UPDATE                             
         AP    0(8,R5),BKONET      UPDATE NET ORDERED                           
*                                                                               
         LA    R5,BILLCD                                                        
         LA    R5,0(R6,R5)                                                      
         AP    0(8,R5),BKOCD       CD ORDERED                                   
*                                                                               
RBK24    DS    0H                                                               
*                                                                               
         B     RBK12               GO DO NEXT ELEM                              
*                                                                               
         DROP  R2                                                               
*                                                                               
RBK25    DS    0H                                                               
*                                                                               
         LA    R2,BKELEM                                                        
         USING BKELEM,R2                                                        
*                                                                               
         MVI   ELCODE,X'23'        INSERTION MONTH BUCKETS                      
         CLI   0(R2),X'23'                                                      
         BE    RBK27                                                            
*                                                                               
RBK26    BAS   RE,NEXTEL                                                        
         BNE   RBK30                                                            
*                                                                               
RBK27    CLI   BKTYPE,C'I'         SEE IF INS MTH                               
         BNE   RBK26                                                            
*                                                                               
         LA    R3,BKYM                                                          
         BAS   RE,FINDISP                                                       
         BNE   RBK26                                                            
*                                                                               
         L     R6,DISP                                                          
*                                                                               
*        UPDATE PAID BUCKETS                                                    
*                                                                               
         LA    R5,INSG             START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) PAID BUCKET TO BE UPDATED                    
         AP    0(8,R5),BKPGRS      UPDATE PAID GROSS                            
*                                                                               
         LA    R5,INSN             START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) BUCKET TO BE UPDATED                         
         AP    0(8,R5),BKPNET      UPDATE PAID NET                              
*                                                                               
         LA    R5,INSCD            START OF BUCKETS                             
         LA    R5,ACCMLEN/3(R6,R5) BUCKET TO BE UPDATED                         
         AP    0(8,R5),BKPCD       UPDATE PAID CD                               
*                                                                               
*        UPDATE ORDERED BUCKETS                                                 
*                                                                               
         CLI   PLNCSTSW,C'P'       IF DOING PLANNED COSTS                       
         BNE   RBK28                  SKIP ORDERED $'S IN SOME CASES            
*                                                                               
         CLI   WORKPCYN,C'Y'       IF NOT DOING PC BILLING                      
         BNE   RBK28                  USE ORDERED DOLLARS                       
*                                                                               
         OC    CPEEBEFD,CPEEBEFD   IF NO BILLING EFFECTIVE DATE                 
         BZ    RBK28                  USE ORDERED DOLLARS                       
*                                                                               
         CLC   BKYM,CPEEBEFD       IF BUCKET MONTH IS BEFORE BEFD               
         BL    RBK28                  USE ORDERED DOLLARS                       
*                                                                               
         OC    CPEEACTD,CPEEACTD   IF NO ACTUALIZATION DATE                     
         BZ    RBK29                  SKIP ORDERED DOLLARS                      
*                                                                               
         CLC   BKYM,CPEEACTD       IF AFTER ACTUALIZATION DATE                  
         BH    RBK29                  SKIP ORDERED DOLLARS                      
*                                                                               
RBK28    DS    0H                                                               
*                                                                               
         LA    R5,INSG             START OF BUCKETS                             
         LA    R5,0(R6,R5)         BUCKET TO UPDATE                             
         AP    0(8,R5),BKOGRS      UPDATE GROSS ORDERED                         
*                                                                               
         LA    R5,INSN                                                          
         LA    R5,0(R6,R5)         BUCKET TO UPDATE                             
         AP    0(8,R5),BKONET      UPDATE NET ORDERED                           
*                                                                               
         LA    R5,INSCD                                                         
         LA    R5,0(R6,R5)                                                      
         AP    0(8,R5),BKOCD       CD ORDERED                                   
*                                                                               
RBK29    DS    0H                                                               
*                                                                               
         B     RBK26               GO DO NEXT 23 ELEM                           
*                                                                               
         DROP  R2                                                               
*                                                                               
RBK30    DS    0H                                                               
*                                                                               
         CLI   PLNCSTSW,C'P'       DONE IF NOT DOING PLANNED COSTS              
         BNE   RBK40                                                            
*                                                                               
         CLI   WORKPCYN,C'Y'       DONE IF NOT DOING PC BILLING                 
         BNE   RBK40                                                            
*                                                                               
*        HANDLE PLANNED COSTS BUCKETS                                           
*                                                                               
         MVI   ELCODE,X'42'        FIND REGULAR PLANNED COST BUCKETS            
         LA    R2,BKELEM                                                        
         USING BKELEM,R2                                                        
*                                                                               
         CLI   0(R2),X'42'                                                      
         BE    RPC15                                                            
*                                                                               
RPC12    BAS   RE,NEXTEL                                                        
*                                                                               
         BNE   RPC25               NO MORE ELEMENTS                             
*                                                                               
RPC15    LA    R3,BKYM                                                          
*                                                                               
         BAS   RE,FINDISP                                                       
         BNE   RPC12                                                            
*                                                                               
RPC20    DS    0H                                                               
*                                                                               
         OC    CPEEBEFD,CPEEBEFD   SKIP IF NO BILLING EFD                       
         BZ    RPC24                                                            
*                                                                               
         CLC   BKYM,CPEEBEFD       IF BUCKET MONTH IS AFTER BEFD                
         BL    RPC24                                                            
         CLC   BKYM,CPEEACTD       AND AFTER ACTUALIZATION DATE                 
         BNH   RPC24                  USE PLANNED COST DOLLARS                  
*                                                                               
         L     R6,DISP             DISPLACEMENT INTO INTERNAL BUCKETS           
*                                                                               
         LA    R5,BILLG            START OF BUCKETS                             
         LA    R5,0(R6,R5)         MONTH BUCKET TO BE UPDATED                   
         AP    0(8,R5),BKOGRS         UPDATE GROSS ORDERED                      
*                                                                               
         LA    R5,BILLN                                                         
         LA    R5,0(R6,R5)                                                      
         AP    0(8,R5),BKONET         UPDATE NET ORDERED                        
*                                                                               
         LA    R5,BILLCD                                                        
         LA    R5,0(R6,R5)                                                      
         AP    0(8,R5),BKOCD       CD ORDERED                                   
*                                                                               
RPC24    DS    0H                                                               
*                                                                               
         B     RPC12               GO DO NEXT ELEM                              
*                                                                               
         DROP  R2                                                               
*                                                                               
RPC25    DS    0H                                                               
*                                                                               
         LA    R2,BKELEM                                                        
         USING BKELEM,R2                                                        
*                                                                               
         MVI   ELCODE,X'43'        PLANNED COST INSERTION MONTH BUCKETS         
         CLI   0(R2),X'43'                                                      
         BE    RPC27                                                            
*                                                                               
RPC26    BAS   RE,NEXTEL                                                        
         BNE   RPC30                                                            
*                                                                               
RPC27    CLI   BKTYPE,C'I'         SEE IF INS MTH                               
         BNE   RPC29                                                            
*                                                                               
         LA    R3,BKYM                                                          
         BAS   RE,FINDISP                                                       
         BNE   RPC29               NO MORE ELEMENTS                             
*                                                                               
         OC    CPEEBEFD,CPEEBEFD   SKIP IF NO BILLING EFD                       
         BZ    RPC29                                                            
*                                                                               
         CLC   BKYM,CPEEBEFD       IF BUCKET MONTH IS AFTER BEFD                
         BL    RPC29                                                            
         CLC   BKYM,CPEEACTD       AND AFTER ACTUALIZATION DATE                 
         BNH   RPC29                  USE PLANNED COST DOLLARS                  
*                                                                               
         L     R6,DISP                                                          
         LA    R5,INSG                                                          
         AR    R5,R6                                                            
*                                                                               
         AP    0(8,R5),BKOGRS      UPDATE GROSS ORDERED                         
*                                                                               
         LA    R5,INSN                                                          
         AR    R5,R6                                                            
         AP    0(8,R5),BKONET                                                   
*                                                                               
         LA    R5,INSCD                                                         
         AR    R5,R6                                                            
         AP    0(8,R5),BKOCD                                                    
*                                                                               
RPC29    DS    0H                                                               
*                                                                               
         B     RPC26               GO DO NEXT 43 ELEM                           
*                                                                               
RPC30    DS    0H                                                               
*                                                                               
RBK40    DS    0H                  GO DO NEXT BUCKET REC                        
         SP    CTR,=P'1'                                                        
         BP    RBK3                                                             
*                                                                               
         DROP  R2,R8,R4                                                         
*                                                                               
         EJECT                                                                  
RBILLS   DS    0H                  READ BILL RECS                               
*                                  FOR EACH PRD/EST IN CPELIST                  
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'08'                                                      
*                                                                               
RBL1     LA    R4,CPELIST                                                       
         USING CPELISTD,R4         ESTABLISH CPELIST                            
*                                                                               
RBL1A    CLI   0(R4),0             END OF LIST                                  
         BE    WRBUCS                                                           
         CLI   CPEDONE,X'FF'       SEE IF I'VE DONE THIS ENTRY                  
         BNE   RBL1C               NO                                           
RBL1B    LA    R4,CPELISTL(R4)                                                  
         B     RBL1A                                                            
*                                                                               
RBL1C    MVC   KEY+4(8),CPECLT     SET CLT/PRD/EST                              
         XC    KEY+12(4),KEY+12    CLEAR OLD MOS                                
         ZIC   R1,CPEDONE          DISPLACEMENT                                 
         ST    R1,DISP                                                          
         MVI   CPEDONE,X'FF'       SO I'LL BYPASS THIS ENTRY NEXT               
RBL2     BAS   RE,HIGH                                                          
         B     RBL5                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
RBL3     BAS   RE,SEQ                                                           
RBL5     CLC   KEY(4),KEYSAVE      CHK FOR MED                                  
         BNE   WRBUCS              END OF CLT GO WRITE ACCUMS                   
**NEW 4/5/91                                                                    
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLC   FATIOCNT,SVMAXIO                                                 
         BNL   STOP                                                             
         DROP  R1                                                               
*                                                                               
         CLC   KEY+4(8),KEYSAVE+4  CHK FOR CLT/PRD/EST                          
         BNE   RBL1B               NOT FOUND GO DO NEXT CLT/PRD/EST             
*                                                                               
RBL10    LA    R3,KEY+12           MOS                                          
         BAS   RE,FINDISP                                                       
         BNE   RBL3                OUT OF PERIOD                                
         BAS   RE,GETREC                                                        
         CLI   PBRETAIL,X'41'      SKIP CORP SUMMARY BILLS - RETAIL             
         BE    RBL3                                                             
*                                                                               
         GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
*                                                                               
*  SET "EFFECTIVE" VALUES INTO PBILLREC                                         
*                                                                               
         MVC   PBILLGRS,PPBVEBG                                                 
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                                                               
         L     R6,DISP                                                          
         LA    R5,BILLG                                                         
         LA    R5,ACCMLEN/3*2(R6,R5)                                            
         AP    0(8,R5),PBILLGRS                                                 
         LA    R5,BILLCD                                                        
         LA    R5,ACCMLEN/3*2(R6,R5)                                            
         AP    0(8,R5),PPBVEBC                                                  
         AP    PBILLNET,PPBVEBC       G-AC-CD + CD =NET                         
         LA    R5,BILLN                                                         
         LA    R5,ACCMLEN/3*2(R6,R5)                                            
         AP    0(8,R5),PBILLNET                                                 
         B     RBL3                GO DO NEXT BILL                              
WRBUCS   LA    R3,1                PAGE 1                                       
         LA    R4,BILLG            BUFFER ADDR                                  
         BAS   RE,WRTEMP           WRITE TO TEMPSTR                             
         B     DP5                 GO DISPLAY - BYPASS READ OF TWA              
         EJECT                                                                  
DISPLAY  DS    0H                  READ BUCKETS FOR TWA                         
         LA    R3,1                PAGE 1                                       
         LA    R4,BILLG            BUFFER ADDR                                  
         BAS   RE,RDTEMP           READ FROM TEMPSTR                            
*                                                                               
DP5      DS    0H                                                               
         XC    FISHD1,FISHD1                                                    
         FOUT  FISHD1H                                                          
         MVC   FISHD2(80),=CL80'PRD/EST  %PAID      ORDERED         PAIX        
               D       UNPAID       BILLED    BILLABLE '                        
*                                                                               
         CLI   PLNCSTSW,C'P'       IF DOING PLANNED COSTS                       
         BNE   *+10                                                             
         MVC   FISHD2+20(7),=CL7'PLN/ORD'                                       
*                                                                               
         CLI   MTHSW,C'I'           SEE IF DOING INS MTHS                       
         BNE   *+10                 NO BILLING DATA AVAILABLE                   
         XC    FISHD2+60(19),FISHD2+60                                          
*                                                                               
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    DP8                                                              
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BE    DP8                                                              
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    DP8                                                              
         CLC   SVCLT,=C'ALL'                                                    
         BNE   DP10                                                             
DP8      MVC   FISHD2(14),=CL14'CLT/PRD/EST'                                    
DP10     FOUT  FISHD2H                                                          
*                                                                               
         ZAP   TOTORD,=P'0'                                                     
         ZAP   TOTPD,=P'0'                                                      
         ZAP   TOTBLD,=P'0'                                                     
*                                                                               
         LA    R3,FISOT01H              SET UP OUTPUT SCREEN                    
         LA    R1,CPELIST                                                       
         USING CPELISTD,R1         ESTABLISH CLT/PRD/EST LIST                   
*                                                                               
         SR    R6,R6               USED FOR DISPLACEMENT                        
*                                                                               
LPOUT    CLI   0(R1),0                                                          
         BE    TOTLP               LAST CLT/PRD/EST                             
*                                                                               
         MVC   MN(3),CPEPRD        PRD                                          
         MVI   SLSH,C'/'                                                        
*                                                                               
         MVC   HALF,CPEEST         ESTIMATE                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  YR(3),DUB           EST                                          
*                                                                               
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    DP15                                                             
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BE    DP15                                                             
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    DP15                                                             
         CLC   SVCLT,=C'ALL'                                                    
         BNE   BLKSET                                                           
*                                                                               
DP15     MVC   MN,CPECLT           CLIENT                                       
         MVI   SLSH,C'/'                                                        
*                                                                               
         MVC   YR(3),CPEPRD        PRD                                          
         MVI   YR+3,C'/'                                                        
*                                                                               
         MVC   HALF,CPEEST                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  YR+4(3),DUB           EST                                        
*                                                                               
         EJECT                                                                  
*        BUCKET LOOP                                                            
*                                                                               
BLKSET   DS    0H                                                               
*                                                                               
         LA    R7,BILLG                                                         
*                                                                               
         CLI   MTHSW,C'B'          SEE IF DOING BILLABLE MTHS                   
         BE    *+8                                                              
         LA    R7,INSG             MUST BE INSERTION MTHS                       
*                                                                               
         CLI   DOLSW,C'1'          GROSS                                        
         BE    BLKSX                                                            
*                                                                               
         CLI   DOLSW,C'2'          GROSS - CD                                   
         BNE   BLKS10                                                           
*                                                                               
         LA    R5,ACCMLEN*2(R7)                                                 
*                                                                               
BLKS3    LR    R4,R7                                                            
*                                                                               
         LTR   R6,R6               SEE IF DOING FIRST CLT/PRD/EST               
         BNZ   BLKSX               NO DON'T SUBTRACT AGAIN                      
*                                                                               
         LA    R2,ACCMLEN/8                                                     
*                                                                               
BLKS5    SP    0(8,R4),0(8,R5)                                                  
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R2,BLKS5                                                         
*                                                                               
         B     BLKSX                                                            
*                                                                               
BLKS10   LA    R7,ACCMLEN(R7)                                                   
*                                                                               
         CLI   DOLSW,C'3'          NET                                          
         BE    BLKSX                                                            
*                                                                               
         CLI   DOLSW,C'4'          NET-CD                                       
         BNE   BLKS20                                                           
*                                                                               
         LA    R5,ACCMLEN(R7)                                                   
         B     BLKS3                                                            
*                                                                               
BLKS20   CLI   DOLSW,C'5'          CD                                           
         BNE   BLKS25                                                           
*                                                                               
         LA    R7,ACCMLEN(R7)                                                   
*                                                                               
         B     BLKSX                                                            
*                                                                               
BLKS25   DC    H'0'                                                             
*                                                                               
BLKSX    DS    0H                                                               
*                                                                               
         LA    R8,3                                                             
         LA    R7,0(R6,R7)                                                      
         LA    R2,WORKSP1                                                       
*                                                                               
TBLPROC  ZAP   0(8,R2),0(8,R7)          PUT AMT INTO WORK                       
         LA    R7,104(R7)               GET NEXT LEVEL -ORD-PD-BLLD             
         LA    R2,8(R2)                 GET NEXT WORK                           
         BCT   R8,TBLPROC                                                       
*                                                                               
         AP    TOTORD,WORKSP1      ADD TO TOTAL LINE                            
         AP    TOTPD,WORKSP2                                                    
         AP    TOTBLD,WORKSP3                                                   
*                                                                               
CALC5    ZAP   WORKSP4(8),WORKSP1(8)    MOVE ORD TO UNPD                        
         SP    WORKSP4(8),WORKSP2(8)    UNPD = ORD-PAID                         
         ZAP   WORKSP5(8),WORKSP1(8)    MOVE ORD TO UNBL                        
         SP    WORKSP5(8),WORKSP3(8)    UNBL = ORD- BILLED                      
*                                                                               
         ZAP   WORKSP1(8),WORKSP1(8)                                            
         BNZ   DUD                      IF NOT ZERO GO TO DIVIDE                
*                                                                               
         ZAP   WORKSP6(8),WORKSP1(8)   ZERO DIVISOR                             
         B     MFLDS                    GO TO MOVE FIELDS                       
*                                                                               
DUD      ZAP   WORKSP6(16),WORKSP2(8)   MOVE PAID TO DIVIDE                     
         MP    WORKSP6(16),=P'1000'     SET DECIMAL POSITION FOR 3 PLCS         
         DP    WORKSP6(16),WORKSP1      PERCENT PAID                            
*                                                                               
MFLDS    LA    R0,5                     SET LOOP                                
         LA    R8,WORKSP1                 GET BEGINING OF WORK AREA             
MFLDS2   ZAP   DUB,0(8,R8)                                                      
*                                                                               
         CP    DUB,=P'0'                                                        
         BL    MFLDS3                                                           
*                                                                               
         AP    DUB,=P'50'          ROUND                                        
         B     MFLDS4                                                           
*                                                                               
MFLDS3   SP    DUB,=P'50'               NEGATIVE ROUNDING                       
*                                                                               
MFLDS4   DP    DUB,=P'100'                                                      
         ZAP   0(8,R8),DUB(6)                                                   
         LA    R8,8(R8)                 BUMP TO NEXT AMT                        
         BCT   R0,MFLDS2                                                        
*                                                                               
*        EDIT  LINE                                                             
*                                  NO PAID PERCENT FOR ALL CLIENTS              
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    MFLD5                                                            
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BE    MFLD5                                                            
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE  LIST                    
         BE    MFLD5                                                            
         CLC   SVCLT,=C'ALL'                                                    
         BE    MFLD5                                                            
         CP    WORKSP6(8),=P'0'           CHK FOR PCT PAID                      
         BE    MFLD5               ZERO - SKIP                                  
         EDIT  (P8,WORKSP6),(7,PDPC),1,MINUS=YES                                
         CP    WORKSP6(8),=P'0'           CHK FOR NEG PCT PAID                  
         BH    MFLD5                                                            
         MVI   PDPC+7,C'-'                                                      
*              SINCE EDIT DOESN'T DO MINUS FOR 1 DECIMAL                        
MFLD5    DS    0H                                                               
         EDIT  (P8,WORKSP1),(13,ORD),0,COMMAS=YES,MINUS=YES                     
         EDIT  (P8,WORKSP2),(13,PD),0,COMMAS=YES,MINUS=YES                      
*                                                                               
         TM    CPEESTA,X'80'        FOR TEST EST                                
         BZ    MFLD6                                                            
*                                                                               
         MVC   UNPD+6(6),=C'*TEST*'                                             
*                                                                               
         B     MFLD6B                                                           
*                                                                               
MFLD6    EDIT  (P8,WORKSP4),(13,UNPD),0,COMMAS=YES,MINUS=YES                    
*                                                                               
MFLD6B   DS    0H                                                               
*                                                                               
         CLI   MTHSW,C'I'         NO BILLED/BILLABLE FOR INS MTHS               
         BE    EDLP10                                                           
*                                                                               
         EDIT  (P8,WORKSP3),(13,BILD),0,COMMAS=YES,MINUS=YES                    
*                                                                               
         TM    CPEESTA,X'80'        FOR TEST EST                                
         BZ    MFLD7                                                            
*                                                                               
         MVC   BLBL+6(6),=C'*TEST*'                                             
*                                                                               
         B     EDLP10                                                           
*                                                                               
MFLD7    EDIT  (P8,WORKSP5),(12,BLBL),0,COMMAS=YES,MINUS=YES                    
*                                                                               
EDLP10   LA    R1,CPELISTL(R1)     NEXT CLT/PRD/EST                             
         LA    R6,8(R6)            SET DISPLACEMENT                             
*                                                                               
         FOUT  (R3),OUTAREA,80                                                  
*                                                                               
         XC    OUTAREA,OUTAREA                                                  
         LA    R3,88(R3)                                                        
*                                                                               
         B     LPOUT                                                            
         EJECT                                                                  
TOTLP    DS    0H                  WAS TOTAL ROUTINE                            
*                                                                               
*                                                                               
ENDJOB   LA    R4,FISOT14H+88                                                   
EJ1      OC    8(80,R3),8(R3)                                                   
         BZ    ENJB                                                             
LOOPED   CR    R3,R4                                                            
         BE    ENJB                                                             
         XC    8(80,R3),8(R3)                                                   
         FOUT  (R3)                                                             
         LA    R3,88(R3)                                                        
         B     EJ1                                                              
ENJB     DS    0H                                                               
         LA    R2,FISMDIAH                                                      
         MVC   FISEMSG,=CL60'** NO DATA FOUND **'                               
         CLI   DATASW,0                                                         
         BE    MODEXIT                                                          
         MVC   FISEMSG,=CL60'** END OF REQUESTED DATA **'                       
         OC    PREVKEY,PREVKEY                                                  
         BZ    MODEXIT                                                          
         MVC   PREVDSW,DATASW                                                   
         LA    R2,FISEND1H                                                      
         OI    FISOPTH+1,X'01'     MODIFY OPTIONS FIELD                         
         FOUT  FISOPTH                                                          
         MVC   FISEMSG,=CL60'** HIT ENTER FOR NEXT PAGE **'                     
MODEXIT  OI    6(R2),X'C0'                                                      
         MVC   LDOLSW,DOLSW        SAVE DOLSW                                   
         FOUT  FISEMSGH                                                         
         XMOD1                                                                  
         EJECT                                                                  
SPACES   DC    C'                         '                                     
         EJECT                                                                  
FINDISP  NTR1                                                                   
         CLC   FISSTDE(2),=C'ES'                                                
         BE    FDX                 SKIP DATE CHK                                
*                                  R3 HAS ADDR OF BUCKET YM                     
*                                  OR BILL MOS                                  
         CLC   0(2,R3),SVSTRTB                                                  
         BE    FDX                                                              
         BH    FD4                                                              
         CLI   PRSW,1                                                           
         BE    FDX                 YES                                          
         B     FDERR                                                            
*                                                                               
FD4      CLC   0(2,R3),SVENDB      CHK END DATE                                 
         BNH   FDX                                                              
         CLI   SUBSW,1                                                          
         BNE   FDERR                                                            
FDX      SR    RE,RE                                                            
FDERR    LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
*                   DATA MANAGER INTERFACE FOR TEMPSTR WRITE                    
         SPACE 2                                                                
WRTEMP   ST    RE,FULL                                                          
         XC    DMCB+8(2),DMCB+8                                                 
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERMINAL NUMBER                       
         STC   R3,DMCB+8           PAGE                                         
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R4),(TERMNAL,0)            
         SPACE 2                                                                
         L     RE,FULL                                                          
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                TEMPSTR WRITE ERROR                          
         SPACE 2                                                                
RDTEMP   ST    RE,FULL                                                          
         XC    DMCB+8(2),DMCB+8                                                 
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERMINAL NUMBER                       
         STC   R3,DMCB+8           PAGE                                         
         MVI   DMCB+9,X'FF'        FOR FULL PAGE READ                           
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,(R4),(TERMNAL,0)           
         L     RE,FULL                                                          
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                TEMPSTR WRITE ERROR                          
*                                                                               
         TITLE 'PPFIS03 - PRINTPAK FIS  EST TOTAL DISPLAY - GETNCLT'            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO GET NEXT CLIENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
GETNCLT  NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTKEY,R4          ESTABLISH CLIENT REC KEY                     
*                                                                               
         MVC   PCLTKAGY,AGYALPHA   SET AGENCY                                   
         MVC   PCLTKMED,SVMED      SET MEDIA                                    
         MVI   PCLTKRCD,X'02'      SET RECORD ID                                
*                                                                               
GETNCLP  DS    0H                                                               
*                                                                               
         MVC   PCLTKCLT,WORKCLT    SET CURRENT CLIENT CODE                      
         MVI   PCLTKCLT+L'PCLTKCLT,X'FF'  FORCE NEXT CLIENT                     
*                                                                               
         BAS   RE,HIGH             READ POINTER FOR NEXT CLIENT                 
*                                                                               
         XC    WORKCLT,WORKCLT     CLEAR WORKCLT CODE                           
*                                                                               
         GOTO1 GETFACT,DMCB,0      CHECK ON # OF IO'S SO FAR                    
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
*                                                                               
         CLC   FATIOCNT,SVMAXIO    OVER THE LIMIT OF IO'S                       
         BNL   GETNCER1                                                         
*                                                                               
         CLC   PCLTKEY(PCLTKCLT-PCLTKEY),KEYSAVE                                
         BNE   GETNCX              DONE ON CHANGE IN REC TYPE/MED/AGY           
*                                                                               
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    GETNC9                                                           
*                                                                               
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    GETNC10                                                          
*                                                                               
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BE    GETNC2                                                           
*                                                                               
*        ALL OR SINGLE CLIENT                                                   
*                                                                               
         B     GETNCFD             RETURN CLIENT CODE                           
*                                                                               
         DROP  R4                                                               
*                                                                               
*        OFFICE OR RANGE OF OFFICES                                             
*                                                                               
GETNC2   DS    0H                                                               
*                                                                               
         BAS   RE,GETREC           READ IN CLIENT RECORD                        
*                                                                               
         BAS   RE,CKTRA            TRAFFIC OFFICE REPLACEMENT RTN               
*                                                                               
         CLI   SVCLT+1,C'-'        IF DOING ALL BUT ONE OFFICE                  
         BNE   GETNC3                                                           
*                                                                               
         CLC   PCLTOFF(1),SVCLT+2     BYPASS CLIENTS IN THIS OFFICE             
         BE    GETNCCN                                                          
*                                                                               
         B     GETNCFD             RETURN CLIENT CODE                           
*                                                                               
*        RANGE OF OFFICES                                                       
*                                                                               
GETNC3   CLI   SVCLT+2,0           SEE IF DOING RANGE OF OFFICES                
         BNH   GETNC5              NO                                           
*                                                                               
*        TRANSLATE PCLTOFF TO 2 CH OFFICE CODE                                  
*                                                                               
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF      CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT,6(RA)                                                     
*                                                                               
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD          A("SECRET BLOCK")                            
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         BNE   GETNCCN             SKIP CLIENT IF INVALID OFFICE CODE           
*                                                                               
         LA    R1,WORK2            ESTABLISH OFFICER CONTROL BLOCK              
         USING OFFICED,R1                                                       
*                                                                               
         CLC   OFCOFC2,SVOFCST2    MAKE SURE OFFICE IN RANGE                    
         BL    GETNCCN             LOW SKIP                                     
         CLC   OFCOFC2,SVOFCEN2                                                 
         BH    GETNCCN             HIGH SKIP                                    
*                                                                               
         B     GETNCFD             PROCESS                                      
*                                                                               
         DROP  R1                                                               
*                                                                               
GETNC5   CLC   PCLTOFF(1),SVCLT+1  ONE OFFICE - MUST MATCH                      
         BNE   GETNCCN             NO SKIP                                      
*                                                                               
         B     GETNCFD             RETURN THIS CLIENT                           
*                                                                               
*        OFFICE LIST                                                            
*                                                                               
GETNC9   DS    0H                  HERE FOR OFFICE LIST CHECKING                
*                                                                               
         BAS   RE,GETREC           MUST READ CLT                                
*                                                                               
         BAS   RE,CKTRA            TRAFFIC OFFICE REPLACEMENT RTN               
*                                                                               
         MVI   LIMITSW,C' '        SET TO USE OFFICER TO "SELECT"               
         BAS   RE,PPCLIVER         CALL OFFICER                                 
         BE    GETNCFD             CHECK FOR SECURITY - CLIENT OKAY             
*                                                                               
         B     GETNCCN             SKIP THIS CLT                                
*                                                                               
*        BILLING GROUP                                                          
*                                                                               
GETNC10  DS    0H                  BILLING GROUP (&) TESTING                    
*                                                                               
         BAS   RE,GETREC           READ IN CLIENT RECORD                        
*                                                                               
         CLC   PCLTBLGP,SVCLT+1    MAKE SURE CORRECT BILLING GROUP              
         BNE   GETNCCN             NO - SKIP THIS CLT                           
*                                                                               
         B     GETNCFD             ACCEPT THIS CLT                              
*                                                                               
*        CLIENT IS GOOD CANDIDATE                                               
*              CHECK THAT SECURITY ALLOWS USER TO VIEW CLIENT                   
*                                                                               
GETNCFD  DS    0H                                                               
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    GETNC90             OKAY - NO LIMIT ACCESS                       
*                                                                               
         MVI   LIMITSW,C'Y'        SET TO USE OFFICER FOR SECURITY              
*                                                                               
         BAS   RE,PPCLIVER         CALL OFFICER                                 
         BNE   GETNCCN             SKIP THIS CLT                                
*                                                                               
         B     GETNC90             CLIENT OKAY FOR DISPLAY                      
*                                                                               
GETNCCN  MVC   WORKCLT,KEY+4                                                    
         B     GETNCLP             GO CHK NEXT CLIENT                           
*                                                                               
*        CLIENT OKAY FOR DISPLAY                                                
*                                                                               
GETNC90  MVC   WORKCLT,KEY+4       PROCESS THIS CLIENT                          
*                                                                               
*        IF DISPLAYING PLANNED COSTS                                            
*              NEED TO GET B2B PROFILE FOR BILLING EFFECTIVE DATE               
*                                                                               
         CLI   PLNCSTSW,C'P'       SKIP IF NOT DISPLAYING PLANNED COSTS         
         BNE   GETNC99                                                          
*                                                                               
*        READ B2B PROFILE FOR CLIENT                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PB2B'    REQUEST B2B PROFILE                          
         NI    WORK,X'FF'-X'40'    MAKE 'P' LOWERCASE                           
*                                  NEEDED FOR 3 CH PROF IDS                     
         MVC   WORK+4(2),PCLTKAGY  AGENCY                                       
         MVC   WORK+6(1),PCLTKMED  MEDIA                                        
         MVC   WORK+7(3),PCLTKCLT  CLIENT                                       
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF  CLIENT OFFICE                                
*                                                                               
         L     RF,ACOMFACS         COMFACS ADDRESS                              
         L     R0,CDATAMGR-COMFACSD(RF)    DATAMGR ADDRESS                      
         L     RF,CGETPROF-COMFACSD(RF)    GETPROF ADDRESS                      
*                                                                               
         GOTO1 (RF),DMCB,WORK,B2BPROF,(R0),0,0                                  
*                                                                               
         MVC   WORKPCYN,B2BPROF+12   SAVE CLIENT PC BILLING OPTION              
         MVC   WORKCBLD,B2BPROF+13   SAVE CLIENT BILLING EFF DATE               
*                                                                               
         OC    WORKCBLD,WORKCBLD   SKIP IF NONE                                 
         BZ    GETNC95                                                          
*                                                                               
         CLI   WORKCBLD,80         SET CENTURY                                  
         BNL   GETNC95                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WORKCBLD         SET CENTURY                                  
         AHI   RF,100                                                           
         STC   RF,WORKCBLD                                                      
*                                                                               
GETNC95  DS    0H                                                               
*                                                                               
GETNC99  DS    0H                                                               
*                                                                               
         B     GETNCX                                                           
*                                                                               
GETNCER1 DS    0H                  TOO MANY IO'S                                
*                                                                               
         MVC   WORKCLT(3),=X'FFFFFF'  TOO MANY IO'S                             
*                                                                               
         B     GETNCX              EXIT                                         
*                                                                               
GETNCX   XIT1                                                                   
*                                                                               
         TITLE 'PPFIS03 - PRINTPAK FIS  EST TOTAL DISPLAY'                      
CKTRA    NTR1                                                                   
         CLI   TRFAGSW,C'Y'        TRAFFIC AGENCY ID ?                          
         BNE   CKTRAX              NO                                           
*                                  SEE IF TRAFFIC OFFICE EXISTS                 
         MVI   ELCODE,X'50'                                                     
         LA    R2,PCLTREC+33                                                    
         BAS   RE,NEXTEL                                                        
         BNE   CKTRAX                                                           
         MVC   PCLTOFF,2(R2)    REPLACE CLT OFFICE WITH TRAFFIC OFFICE          
CKTRAX   XIT1                                                                   
         EJECT                                                                  
*       *************************                                               
******  TEST OFFICE LIST SECURITY  ******                                       
*       *************************                                               
         SPACE 2                                                                
*                                                                               
*NOP*PPCLIVER NTR1 ***** NOTE- I/O AREA M/B IN AREC ******                      
*                                                                               
*                  NOTE - ONLY GO HERE IF SVCLT IS ($N) OFFICE LIST             
*                                                                               
*NOP*    XC    WORK,WORK                                                        
*NOP*    LA    R1,WORK                                                          
*NOP*    USING OFFICED,R1                                                       
*NOP*   MVI   OFCSYS,C'P'                                                       
*NOP*    MVC   OFCAUTH,SVCLT    NOTE-SVCLT WILL HAVE OFFICE LIST ($N)           
*NOP*    MVC   OFCAGY,AGYALPHA                                                  
*NOP*    MVC   OFCOFC,PCLTOFF                                                   
*NOP*    DROP  R1                                                               
*NOP*    GOTO1 VOFFICER,DMCB,WORK,ACOMFACS                                      
*NOP*    CLI   0(R1),0                                                          
*NOP*    XIT1                                                                   
*******************************************************                         
*******************************************************                         
         EJECT                                                                  
*                                                                               
PPCLIVER NTR1                   *****  LIMIT ACCESS TESTING   *****             
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         CLI   LIMITSW,C'Y'        DOING SECURITY ?                             
         BE    *+10                YES                                          
         MVC   OFCAUTH,SVCLT       MUST BE "SELECTING" FROM OFC LIST            
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF      CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         CLI   LIMITSW,C'Y'        DOING SECURITY ?                             
         BE    *+10                YES                                          
         MVC   OFCLMT(2),SVCLT     MUST BE "SELECTING" FROM OFC LIST            
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD          A("SECRET BLOCK")                            
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         XIT1                                                                   
         EJECT                                                                  
STOP     DS    0H                                                               
         XC    FISEMSG,FISEMSG                                                  
         MVC   FISEMSG(49),=C'**MAXIMUM FILE READS EXCEEDED - REQUEST SX        
               TOPPED**'                                                        
         FOUT  FISEMSGH                                                         
         XC    PREVKEY,PREVKEY                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
*                  FARMABLE CODE                                                
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPBVALD           NEW DSECT FOR PPBVAL ROUTINE                 
*                                                                               
       ++INCLUDE PPFISWRK          CONTAINS NEW DESCT FOR PBILLREC              
