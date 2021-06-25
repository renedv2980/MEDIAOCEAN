*          DATA SET ACREPCK02S AT LEVEL 009 AS OF 06/18/99                      
*PHASE ACCK02A,*                                                                
*INCLUDE DATVAL                                                                 
         TITLE 'CLEARANCE CHECK REPORT'                                         
ACCK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACCK02*,R9                                                    
         L     RC,0(R1)                                                         
         USING ACWORKD,RC                                                       
         LA    RA,SPACEND                                                       
         USING ACCKD,RA                                                         
         SPACE 1                                                                
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         GOTO1 DATVAL,DMCB,(0,RCDATE),WORK                                      
         GOTO1 DATCON,DMCB,(0,WORK),(1,TODAY)                                   
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   AUTL,MCUTL                                                       
         XC    ID,ID                                                            
         L     R7,ASTOTS           MARK END OF TABLE                            
         ST    R7,ANXTSUM          NEXT SUMMARY ENTRY                           
         MVI   0(R7),0                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD A LIST OF ALL ACC SE NUMBERS AND FILE CODES                   *         
***********************************************************************         
         SPACE 1                                                                
         LA    R2,DKEY                                                          
         USING CTWREC,R2           READ CONTROL FILE                            
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVI   CTWKSYSN,CTWKACC                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,DKEY,AIO                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T READ SYSTEM LIST RECORD                
*                                                                               
         L     R2,AIO                                                           
         LA    R3,ACLST            LIST OF ACC SYS SE NUMBERS                   
         LA    R2,CTWDATA                                                       
         USING SYSELD,R2                                                        
         SR    RF,RF               COUNT ENTRIES                                
         SR    R0,R0                                                            
*                                                                               
ACCK3    CLI   SYSEL,0             EOR                                          
         BE    ACCK7                                                            
         CLI   SYSEL,SYSELQ        X'A4' CONTROL FILE SYSTEM ELEMENT            
         BNE   ACCK5                                                            
         CLI   SYSSYS,X'06'        X'06' IS CODE FOR ACC SYSTEMS                
         BNE   ACCK5                                                            
         MVC   0(1,R3),SYSSEN      SAVE ACCOUNT SE NUMBERS                      
         MVC   1(1,R3),SYSNAME+3   SAVE ACC #                                   
         TM    1(R3),X'F0'         SORT NUMERIC FIRST                           
         BNO   *+8                                                              
         NI    1(R3),X'7F'                                                      
         LA    R3,L'ACLST(R3)                                                   
         XC    0(L'ACLST,R3),0(R3)                                              
         LA    RF,1(RF)            COUNT ENTRIES                                
         STC   RF,ACC#             NUMBER OF ACC FILES                          
         CH    RF,=Y(MXACC)        TOO MANY ACC FILES                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACCK5    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     ACCK3                                                            
         EJECT                                                                  
ACCK7    GOTO1 XSORT,DMCB,(0,ACLST),(RF),3,1,1  SORT BY ACC NUMBER              
         LA    R3,ACLST            ASSIGN RELATIVE NUMBER TO ACC SE'S           
         IC    R0,ACC#                                                          
         SR    RF,RF                                                            
*                                                                               
ACCK9    OI    1(R3),X'80'                                                      
         STC   RF,2(R3)                                                         
         LA    RF,1(RF)                                                         
         LA    R3,L'ACLST(R3)                                                   
         BCT   R0,ACCK9                                                         
         EJECT                                                                  
***********************************************************************         
* READ AND PROCESS ODDS FILE RECORDS                                  *         
***********************************************************************         
         SPACE 1                                                                
ACCK13   BAS   RE,INDX             GET INDEX RECORD                             
         BNE   ACCK25              END OF INDEX RECORDS - DO SUMMARY            
         MVI   OPNSW,C'N'                                                       
*                                                                               
ACCK17   GOTO1 WORKER,DMCB,WKREAD,ABUFF,ID,ODDIO                                
         CLI   8(R1),0             TEST EOF                                     
         BNE   ACCK23                                                           
*                                                                               
         LA    R5,ODDIO            IGNORE ZERO RECORDS                          
         USING ODDD,R5                                                          
         CP    ODDAMT,=P'0'                                                     
         BNE   ACCK18                                                           
         CP    ODDCR,=P'0'                                                      
         BNE   ACCK18                                                           
         CP    ODDCOMO,=P'0'                                                    
         BNE   ACCK18                                                           
         CLI   ODDLEN+1,48         TEST COMMISSION ONLY                         
         BNH   ACCK17              OK  TO  SKIP                                 
         CP    ODDCOMO,=P'0'                                                    
         BE    ACCK17              TRADE BUYS                                   
*                                                                               
ACCK18   CLI   OPNSW,C'Y'                                                       
         BE    *+12                                                             
         BAS   RE,OPNM             OPEN MEDIA FILES/POST AGENCY TOTALS          
         MVI   OPNSW,C'Y'                                                       
         LA    R6,MTOTREC                                                       
         USING MTOTD,R6                                                         
         LA    R5,ODDIO                                                         
         USING ODDD,R5                                                          
         LA    R3,ACLST            GET RELATIVE ACC SE NUMBER                   
         IC    R0,ACC#                                                          
         CLC   ODDSE,0(R3)                                                      
         BE    *+14                                                             
         LA    R3,L'ACLST(R3)                                                   
         BCT   R0,*-14                                                          
         DC    H'0'                INVALID ACC SE                               
*                                                                               
         MVC   MTOTASE,2(R3)       ACC RELATIVE NUMBER                          
         MVC   MTOTAGY,ODDAGY      AGENCY ALPHA                                 
         MVI   MTOTSUB,MTOTSRB     SET REGUALR BUY SUBLINE                      
         MVC   MTOTNME,ODDNAM      NAME                                         
         MVC   MTOTCLR(PZLQ),PZ    INITIALIZE ACCUMS                            
         ZAP   MTOTCLR,ODDAMT      CLEARANCE AMOUNT                             
         SP    MTOTCLR,ODDCR       LESS CASH RECEIPTS                           
         ZAP   MTOTCRS,ODDCR       CASH RECEIPTS                                
         ZAP   MTOTCSH,ODDAMT      CASHPACK TOTAL                               
         CLI   ODDLEN+1,48         TEST COMMISSION ONLY                         
         BNH   *+10                                                             
         ZAP   MTOTCOM,ODDCOMO     COMMISSION ONLY                              
         CLI   ODDLEN+1,ODDLNQ     TEST TRADE BUY BUCKET                        
         BL    *+10                                                             
         ZAP   MTOTTRD,ODDTRADE    TRADE BUYS                                   
         BAS   RE,ADDMS            ADD TO MEDIA SUMMARY                         
*                                                                               
         CP    MTOTTRD,=P'0'       ANY TRADE BUYS                               
         BE    ACCK17                                                           
         MVI   MTOTSUB,MTOTSTB         SET TRADE BUY SUBLINE                    
         MVC   MTOTNME,SPACES                                                   
         MVC   MTOTNME+1(14),=C'* TRADE BUYS *'                                 
         ZAP   DUB,MTOTTRD                                                      
         MVC   MTOTCLR(PZLQ),PZ    INITIALIZE ACCUMS                            
         ZAP   MTOTCLR,DUB         ADD TO CLEARANCE                             
         ZAP   MTOTCSH,DUB                                                      
         ZAP   MTOTAMT,DUB         AND FILE                                     
         BAS   RE,ADDMS            ADD IT TO SUMMARIES                          
         B     ACCK17                                                           
*                                                                               
ACCK23   SR    RF,RF                                                            
         IC    RF,MSCNT                                                         
         LA    R0,MTOTLNQ                                                       
         GOTO1 XSORT,DMCB,(0,AMTOTS),(RF),(R0),3,0  SORT BY AGENCY              
         BAS   RE,REPT             PRINT MEDIA REPORT                           
         L     RF,ANXTSUM          NEXT SUMMARY ENTRY                           
         AH    RF,=Y(SUMLNQ)                                                    
         ST    RF,ANXTSUM                                                       
         MVI   0(RF),0                                                          
         B     ACCK13              GET ANOTHER INDEX                            
*                                                                               
ACCK25   BAS   RE,ALLS             GET OVERALL TOTAL                            
         BAS   RE,PSUM             PRINT SUMMARY                                
XIT      XIT1  1                                                                
         EJECT                                                                  
***********************************************************************         
* WORKER FILE INDEX ROUTINE                                           *         
***********************************************************************         
         SPACE 1                                                                
INDX     NTR1  ,                   GET INDEX RECORD                             
         ICM   R3,15,AINDX         A(CURRENT ENTRY)                             
         BNZ   INDX7               NOT FIRST TIME                               
         LA    R3,INDXLST          LIST ON INDEX ENTRIES                        
         SR    R0,R0                                                            
*                                                                               
INDX3    GOTO1 WORKER,DMCB,WKINDX,ABUFF,ID,ODDIO                                
         CLI   8(R1),0             TEST EOF                                     
         BNE   INDX5                                                            
         LA    RF,ID               FILTER ODDS RECORDS FOR TODAY                
         USING UKRECD,RF                                                        
         CLC   UKDAY,TODAY+2       FOR TODAY ONLY                               
         BNE   INDX3                                                            
         CLI   UKCLASS,C'O'                                                     
         BNE   INDX3                                                            
         CLC   UKSYSPRG,=C'S54'    TEST SPOT                                    
         BE    *+14                                                             
         CLC   UKSYSPRG,=C'P54'    TEST PRINT                                   
         BNE   INDX3                                                            
         MVC   0(L'ID,R3),UKRECD   SAVE THE KEY                                 
         TM    UKSUBPRG-UKRECD(R3),X'F0' SORT NUMERIC FIRST                     
         BNO   *+8                                                              
         NI    UKSUBPRG-UKRECD(R3),X'7F'                                        
         LA    R3,L'ID(R3)                                                      
         MVI   0(R3),X'FF'                                                      
         AH    R0,=H'1'                                                         
         B     INDX3                                                            
*                                                                               
INDX5    LTR   R0,R0               TEST ANY ID'S                                
         BNZ   *+6                                                              
         DC    H'0'                NO ODDS FILES FOR TODAY                      
         LA    RF,L'ID                                                          
         LA    R2,L'UKSYSPRG+L'UKSUBPRG                                         
         LA    R3,UKSYSPRG-UKRECD                                               
         GOTO1 XSORT,DMCB,(0,INDXLST),(R0),(RF),(R2),(R3)                       
         LA    R3,INDXLST-L'ID                                                  
*                                                                               
INDX7    LA    R3,L'ID(R3)         R3 TO NEXT ENTRY                             
         ST    R3,AINDX                                                         
         CLI   0(R3),X'FF'         EOT                                          
         BNE   *+10                                                             
         LTR   RB,RB                                                            
         B     XIT                                                              
         OI    UKSUBPRG-UKRECD(R3),X'80'                                        
         MVC   ID,0(R3)                                                         
         GOTO1 WORKER,DMCB,WKINDX,ABUFF,ID,ODDIO                                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,ID                                                            
         MVC   MSYS(MSYSLNQ),PRTF  SET PRINT FILE ENTRY                         
         CLI   UKSYSPRG,C'P'       TEST PRINT                                   
         BE    *+10                                                             
         MVC   MSYS(MSYSLNQ),SPTF  SET SPOT FILE ENTRY                          
         MVC   MSYSNUM,UKSUBPRG    (IE SPOT 1)                                  
         CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OPEN MEDIA FILES - POST MEDIA TOTAL                                 *         
***********************************************************************         
         SPACE 1                                                                
OPNM     NTR1  ,                                                                
         L     R6,AMTOTS           MARK END OF TABLE                            
         MVI   0(R6),X'FF'                                                      
         MVI   MSCNT,0                                                          
         LA    R5,ODDIO                                                         
         USING ODDD,R5                                                          
         L     R1,AUTL                                                          
         MVC   4(1,R1),ODDUTL      SET UTL                                      
         GOTO1 DATAMGR,DMCB,DMOPEN,MSYSTEM,MSYSOPN,AIO                          
         XC    DKEY,DKEY                                                        
*                                                                               
OPNM3    BAS   RE,GETM             GET MEDIA AGENCY HEADER RECORDS              
         BNE   XIT                 FINISH WITH AGENCY HEADERS                   
         LA    R6,MTOTREC                                                       
         USING MTOTD,R6                                                         
         MVC   MTOTCLR(PZLQ),PZ    INITIALIZE ACCUMS                            
*                                                                               
         L     R2,AIO                                                           
         CLI   MSYSNME,C'P'                                                     
         BNE   OPNM5                                                            
         USING PAGYD,R2            PRINT                                        
         MVC   MTOTAGY,PAGYKAGY    AGENCY ID                                    
         MVI   MTOTSUB,MTOTSRB                                                  
         MVC   MTOTNME,PAGYNAME    AGENCY NAME                                  
         ZAP   MTOTAMT,PAGYPAID    FILE AMOUNT                                  
         B     OPNM7                                                            
*                                                                               
         USING SAGYD,R2            SPOT                                         
OPNM5    MVC   MTOTAGY,AGYKAGY     AGENCY ID                                    
         MVI   MTOTSUB,MTOTSRB                                                  
         MVC   MTOTNME,AGYNAME     AGENCY NAME                                  
         ZAP   MTOTAMT,AGYCLAMT    FILE AMOUNT                                  
*                                                                               
OPNM7    BAS   RE,ADDMS            ADD ENRTY TO MEDIA SUMMARY TABLE             
         B     OPNM3               GET ANOTHER HEADER                           
         EJECT                                                                  
***********************************************************************         
* READ PRINT FILE FOR TOTAL PAID TODAY                                *         
***********************************************************************         
         SPACE 1                                                                
GETM     CLI   MSYSNME,C'P'        TEST PRINT OR SPOT                           
         BNE   GETMS                                                            
GETMP    NTR1  ,                   PRINT                                        
         LA    R2,DKEY                                                          
         USING PAGYD,R2                                                         
         OC    DKEY,DKEY           TEST FIRST TIME                              
         BNZ   GETMP3                                                           
         MVI   PAGYKAGY,C'A'       AGENCY HEADERS START WITH ALPHA              
         MVI   PAGYKRCD,1          RECORD TYPE                                  
         B     GETMP5                                                           
*                                                                               
GETMP3   SR    R1,R1               NEXT AGENCY                                  
         IC    R1,PAGYKAGY+1                                                    
         LA    R1,1(R1)                                                         
         STC   R1,PAGYKAGY+1                                                    
         XC    PAGYKMED,PAGYKMED   CLEAR FOR FIRST MEDIA                        
*                                                                               
GETMP5   GOTO1 DATAMGR,DMCB,DMRDHI,MSYSDIR,DKEY,DIR                             
         CLI   8(R1),0             EOF                                          
         BNE   XIT                                                              
         MVC   DKEY,DIR            SAVE KEY FOR NEXT                            
         CLI   PAGYKRCD,1          RECORD TYPE                                  
         BNE   XIT                                                              
         MVC   DA,DIR+27                                                        
         GOTO1 DATAMGR,DMCB,GETREC,MSYSFIL,DA,AIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         CLC   PAGYTDAY-PAGYD(L'PAGYTDAY,R4),TODAY                              
         BNE   GETMP3                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ SPOT FILE FOR TOTAL PAID TODAY                                 *         
***********************************************************************         
         SPACE 1                                                                
GETMS    NTR1  ,                   PRINT                                        
         LA    R2,DKEY                                                          
         USING SAGYD,R2                                                         
         MVI   AGYKTYPE,6          AGENCY HEADERS START WITH X'06'              
*                                                                               
GETMS3   SR    R1,R1               NEXT AGENCY                                  
         IC    R1,AGYKAGY+1                                                     
         LA    R1,1(R1)                                                         
         STC   R1,AGYKAGY+1                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,MSYSDIR,DKEY,DIR                             
         CLI   8(R1),0             EOF                                          
         BNE   XIT                                                              
         MVC   DKEY,DIR            SAVE KEY FOR NEXT                            
         CLI   AGYKTYPE,6          RECORD TYPE                                  
         BNE   XIT                                                              
         MVC   DA,DIR+14                                                        
         GOTO1 DATAMGR,DMCB,GETREC,MSYSFIL,DA,AIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         CLC   AGYCLDT-SAGYD(L'AGYCLDT,R4),TODAY                                
         BNE   GETMS3                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD MEDIA SUMMARY ENTRY TO MEDIA TABLE                              *         
***********************************************************************         
         SPACE 1                                                                
ADDMS    NTR1  ,                                                                
         L     R6,AMTOTS           R4=MEDIA SUMMARY TABLE                       
         USING MTOTD,R6                                                         
         LA    R5,MTOTREC          R5=CURRENT ENRTY                             
         LA    R0,MXAGY            R0=MAXIMUM AGENCIES ON A MEDIA FILE          
*                                                                               
ADDMS3   CLI   0(R6),X'FF'         TEST END OF TABLE                            
         BE    ADDMS5                                                           
         CLC   MTOTAGY(L'MTOTAGY+L'MTOTSUB),MTOTAGY-MTOTD(R5)                   
         BE    ADDMS7                                                           
         LA    R6,MTOTLNQ(R6)                                                   
         BCT   R0,ADDMS3                                                        
         DC    H'0'                MEDIA TABLE FULL                             
*                                                                               
ADDMS5   MVC   0(MTOTLNQ,R6),0(R5) ADD NEW ENTRY                                
         MVI   MTOTLNQ(R6),X'FF'   MARK NEW END OF TABLE                        
         SR    RF,RF                                                            
         IC    RF,MSCNT            NUMBER OF MEDIA ENTRIES                      
         LA    RF,1(RF)                                                         
         STC   RF,MSCNT                                                         
         B     ADDMS9                                                           
*                                                                               
ADDMS7   LA    R0,MTOTN            ADD TO TABLE ACCUMULATORS                    
         LA    R1,MTOTCLR                                                       
         LA    R2,MTOTCLR-MTOTD(R5)                                             
         AP    0(6,R1),0(6,R2)                                                  
         LA    R1,6(R1)                                                         
         LA    R2,6(R2)                                                         
         BCT   R0,*-14                                                          
*                                                                               
ADDMS9   BAS   RE,ADDSY            ADD TO SYSTEM TABLE                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD TO SYSTEM SUMMARY TABLE                                         *         
***********************************************************************         
         SPACE 1                                                                
ADDSY    NTR1  ,                                                                
         LA    R6,MTOTREC          R6=CURRENT MEDIA ENTRY                       
         USING MTOTD,R6                                                         
         L     R7,ANXTSUM          ADD TO SUMMARY ENTRY                         
         USING SUMD,R7                                                          
         CLI   0(R7),0             TEST FIRST TIME FOR THIS ENTRY               
         BNE   ADDSY5                                                           
         USING SUMD,R7                                                          
         MVC   SUMNME,MSYSNME      SYSTEM NAME                                  
         ZAP   SUMTOT,=P'0'        MEDIA TOTAL                                  
         LA    R3,ACLST            ACC SE LIST                                  
         SR    R0,R0                                                            
         IC    R0,ACC#                                                          
         LA    RF,SUMACC                                                        
*                                                                               
ADDSY3   ZAP   0(6,RF),=P'0'                                                    
         LA    RF,6(RF)                                                         
         BCT   R0,ADDSY3                                                        
*                                                                               
ADDSY5   SR    R1,R1                                                            
         IC    R1,MTOTASE          ACC REALTIVE NUMBER                          
         MH    R1,=Y(L'SUMACC)                                                  
         LA    RF,SUMACC(R1)                                                    
         AP    0(6,RF),MTOTCSH     ADD CASHPACK TOTAL                           
         AP    SUMTOT,MTOTCSH                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT MEDIA REPORT                                                  *         
***********************************************************************         
         SPACE 1                                                                
REPT     NTR1  ,                                                                
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   HEAD5+84(L'MSYSNME),MSYSNME                                      
         LA    R6,MTOTREC          CLEAR ACCUMULATORS FOR TOTAL                 
         USING MTOTD,R6                                                         
         MVC   MTOTCLR(PZLQ),PZ                                                 
         L     R6,AMTOTS           R6=TABLE ENTRY                               
         LA    R5,MTOTREC          R5=TOTAL LINE                                
*                                                                               
REPT3    MVC   P+1(2),MTOTAGY                                                   
         CLI   MTOTSUB,MTOTSRB                                                  
         BE    *+10                                                             
         MVC   P+1(2),SPACES                                                    
         MVC   P+4(L'MTOTNME),MTOTNME                                           
         ZAP   MTOTDIF,MTOTAMT     FILE AMOUNT                                  
         SP    MTOTDIF,MTOTCSH     LESS CASHPAK = DIFFERENCE                    
         LA    R0,MTOTN                                                         
         LA    R1,MTOTCLR          ADD ENTRY TO TOTAL                           
         LA    R2,MTOTCLR-MTOTD(R5)                                             
         SR    RF,RF                                                            
*                                                                               
REPT5    CP    0(6,R1),=P'0'       TEST ACTIVITY                                
         BE    *+8                                                              
         LA    RF,1(R1)                                                         
         AP    0(6,R2),0(6,R1)     ADD TO TOTAL LINE                            
         LA    R1,6(R1)                                                         
         LA    R2,6(R2)                                                         
         BCT   R0,REPT5                                                         
         LTR   RF,RF                                                            
         BZ    REPT7                                                            
         BAS   RE,EDME             EDIT MEDIA LINE                              
         GOTO1 ACREPORT                                                         
*                                                                               
REPT7    LA    R6,MTOTLNQ(R6)                                                   
         CLI   0(R6),X'FF'                                                      
         BNE   REPT3                                                            
*                                                                               
         GOTO1 ACREPORT            SKIP A LINE                                  
         LA    R6,MTOTREC          EDIT AND PRINT THE TOTAL                     
         BAS   RE,EDME             EDIT MEDIA TOTAL                             
         MVC   P+4(12),=C'** TOTAL **'                                          
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* EDIT MEDIA TOTALS                                                 *           
*********************************************************************           
         SPACE 1                                                                
EDME     NTR1  ,                                                                
         LA    R0,6                                                             
         LA    R2,P+30                                                          
         LA    R3,MTOTCLR                                                       
EDME3    EDIT  (P6,0(R3)),(14,0(R2)),2,COMMAS=YES,ZERO=BLANK,MINUS=YES          
         LA    R2,17(R2)                                                        
         LA    R3,6(R3)                                                         
         BCT   R0,EDME3                                                         
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* GET OVERALL TOTALS                                                *           
*********************************************************************           
         SPACE 1                                                                
ALLS     NTR1  ,                                                                
         L     R7,ANXTSUM          INITIALIZE SUMMARY OVERALL TOTAL             
         USING SUMD,R7                                                          
         MVC   SUMNME,=C'*TOTAL*'  SYSTEM NAME                                  
         ZAP   SUMTOT,=P'0'        MEDIA TOTAL                                  
         LA    R3,ACLST            ACC SE LIST                                  
         SR    R0,R0                                                            
         IC    R0,ACC#                                                          
         LA    RF,SUMACC                                                        
*                                                                               
ALLS3    ZAP   0(6,RF),=P'0'                                                    
         LA    RF,6(RF)                                                         
         BCT   R0,ALLS3                                                         
*                                                                               
         L     R5,ASTOTS                                                        
ALLS5    AP    SUMTOT,SUMTOT-SUMD(L'SUMTOT,R5)                                  
         LA    RE,SUMACC-SUMD(R5)                                               
         LA    RF,SUMACC                                                        
         IC    R0,ACC#                                                          
*                                                                               
ALLS7    AP    0(6,RF),0(6,RE)     ADD LINE TO TOTAL                            
         LA    RE,6(RE)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,ALLS7                                                         
*                                                                               
         AH    R5,=Y(SUMLNQ)                                                    
         CR    R5,R7               R5 = THE TOTAL LINE                          
         BL    ALLS5                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT SUMMARY                                                       *         
***********************************************************************         
         SPACE 1                                                                
PSUM     NTR1  ,                                                                
         MVI   RCSUBPRG,1                                                       
         SR    R1,R1                                                            
         IC    R1,ACC#             NUMBER OF ACC FILE                           
         SR    R0,R0                                                            
         D     R0,=F'6'                                                         
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AH    R1,=H'1'                                                         
         STC   R1,NUMPAGE          R0=NUMBER OF SUMMARY PAGES                   
         MVI   PAGENUM,1           PAGE (PASS NUMBER)                           
         LA    R4,1                                                             
*                                                                               
         USING SUMD,R7                                                          
PSUM3    L     R7,ASTOTS           SET HEADLINES                                
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PSUM5    SR    R4,R4                                                            
         IC    R4,PAGENUM                                                       
         BCTR  R4,0                                                             
         LR    R3,R4                                                            
         MH    R3,=Y(L'ACLST*6)                                                 
         LA    R3,ACLST(R3)        R3=ACLST ENTRY                               
         MH    R4,=Y(L'SUMACC*6)                                                
         LA    R4,SUMACC(R4)       R4=ACCUMS                                    
         L     RF,ACREPORT                                                      
         CLI   SUMNME,C'*'         TEST TOTAL LINE                              
         BNE   *+6                                                              
         BASR  RE,RF               SKIP A LINE                                  
         MVC   P+1(7),SUMNME       MEDIA SYSTEM NAME                            
         EDIT  (P6,SUMTOT),(15,P+14),2,COMMAS=YES,ZERO=BLANK,MINUS=YES          
*                                                                               
         LA    R0,6                R0=COLUMN COUNT                              
         LA    R2,P+31             R2=PRINT LINE POSITION                       
         LA    R1,HEAD5+31         R1=HEADLINE POSITION                         
*                                                                               
PSUM9    MVC   4(3,R1),=C'ACC'                                                  
         MVC   9(1,R1),1(R3)       ACC NUMBER                                   
         MVC   L'P(14,R1),=14C'-'                                               
         EDIT  (P6,0(R4)),(14,0(R2)),2,COMMAS=YES,ZERO=BLANK,MINUS=YES          
         LA    R2,17(R2)                                                        
         LA    R1,17(R1)                                                        
         LA    R4,6(R4)                                                         
         LA    R3,L'ACLST(R3)                                                   
         CLI   0(R3),0             TEST END OF LIST                             
         BE    *+8                                                              
         BCT   R0,PSUM9                                                         
*                                                                               
         GOTO1 ACREPORT                                                         
         CLI   SUMNME,C'*'         TEST TOTAL LINE                              
         BE    *+12                                                             
         AH    R7,=Y(SUMLNQ)                                                    
         B     PSUM5                                                            
*                                                                               
         CLC   PAGENUM,NUMPAGE     FINISHED LAST PAGE                           
         BE    XIT                                                              
         SR    R4,R4                                                            
         IC    R4,PAGENUM                                                       
         LA    R4,1(R4)                                                         
         STC   R4,PAGENUM                                                       
         B     PSUM3                                                            
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
         SPACE 1                                                                
MXACC    EQU   20                  MAXIMUM ACCOUNT FILES                        
MXMED    EQU   40                  MAXIMUM MEDIA FILES                          
MXAGY    EQU   20                  MAXIMUM AGENCIES ON MEDIA FILE               
*                                                                               
DATVAL   DC    V(DATVAL)                                                        
*                                                                               
MSCNT    DC    X'00'               NUMBER OF MEDIA ENTRIES                      
AINDX    DC    A(0)                A(CURRENT INDEX ENTRY)                       
AIO      DC    A(IO)                                                            
AMTOTS   DC    A(MTOTS)                                                         
ASTOTS   DC    A(STOTS)                                                         
ABUFF    DC    A(BUFF)                                                          
*                                                                               
CTFILE   DC    CL8'CTFILE'                                                      
WKINDX   DC    CL8'INDEX'                                                       
WKREAD   DC    CL8'READ'                                                        
DMOPEN   DC    CL8'OPEN'                                                        
GETREC   DC    CL8'GETREC'                                                      
*                                                                               
SPTF     DC    CL7'SPOT  ?'                                                     
         DC    CL8'SPOT'                                                        
         DC    CL8'SPTDIR'                                                      
         DC    CL8'SPTFIL'                                                      
         DC    CL17'NSPTDIR NSPTFIL X'                                          
*                                                                               
PRTF     DC    CL7'PRINT ?'                                                     
         DC    CL8'PRINT'                                                       
         DC    CL8'PRTDIR'                                                      
         DC    CL8'PRTFILE'                                                     
         DC    CL17'NPRTDIR NPRTFILEX'                                          
*                                                                               
PZ       DC    (MTOTN)PL6'0'                                                    
PZLQ     EQU   *-PZ                                                             
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS/IO AREAS/                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'**BUFF**'                                                      
BUFF     DC    4500X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    C'**MTOTS*'                                                      
MTOTS    DS    (MXMED)XL(MTOTLNQ)                                               
*                                                                               
         DS    0D                                                               
         DC    C'**SUMM**'                                                      
STOTS    DS    (MXMED)XL(SUMLNQ)                                                
*                                                                               
         DS    0D                                                               
         DC    C'**ODDA**'                                                      
ODDA     DS    1000C                                                            
         EJECT                                                                  
**********************************************************************          
* LOCAL WORKING STORGAE                                              *          
**********************************************************************          
         SPACE 1                                                                
ACCKD    DSECT                                                                  
TODAY    DS    XL3                                                              
ID       DS    CL16                ODDFILE ID                                   
AUTL     DS    A                   A(UTL)                                       
DKEY     DS    CL64                KEY FOR DIRECTORY READ                       
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    XL4                 DISK ADDRESS                                 
ACLST    DS    (MXACC)XL3          ACC SE/ACC FILE #/RELATIVE #                 
ACC#     DS    X                   NUMBER OF ACCFILES                           
*                                                                               
MSYS     DS    0C                                                               
MSYSNME  DS    CL7                 MEDIA SYSTEM NAME / NUMBER                   
         ORG   MSYSNME+L'MSYSNME-1                                              
MSYSNUM  DS    CL1                 NUMBER                                       
MSYSTEM  DS    CL8                 SYSTEM                                       
MSYSDIR  DS    CL8                 DIRECTORY NAME                               
MSYSFIL  DS    CL8                 FILE NAME                                    
MSYSOPN  DS    CL17                OPEN LIST                                    
MSYSLNQ  EQU   *-MSYS                                                           
*                                                                               
ODDIO    DS    CL(ODDLNQ)          ODDFILE RECORD                               
MTOTREC  DS    CL(MTOTLNQ)         MEDIA SUMMARY RECORD                         
ANXTSUM  DS    A                   A(NEXT SUMMARY ENTRY)                        
         SPACE 1                                                                
PAGENUM  DS    X                   SUMMARY PAGE NUMBER                          
NUMPAGE  DS    X                   NUMBER OF SUMMARY PAGES REQUIRED             
OPNSW    DS    CL1                 Y=MEDIA FILE HAS BEEN OPENED                 
*                                                                               
INDXLST  DS    (MXMED)XL(L'ID)     INDEX RECORD LIST                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER AN ODDFILE RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
ODDD     DSECT                                                                  
ODDREC   DS    0C                                                               
ODDLEN   DS    XL4                                                              
ODDACC   DS    CL1                 ACCOUNTING AGENCY CODE                       
ODDID    DS    CL2                 CONTL AGENCY ID                              
ODDUTL   DS    CL1                 UTL NUMBER                                   
ODDAGY   DS    CL2                 SPOT OR PRINT AGENCY CODE                    
ODDAMT   DS    PL6                 AMOUNT FROM CLEARANCE AC54                   
ODDNAM   DS    CL24                AGENCY NAME                                  
ODDCR    DS    PL5                 CASH RECEIPT                                 
ODDSE    DS    CL1                 ACCOUNT SE NUMBER                            
ODDCOMO  DS    PL6                 COMMISSION ONLY                              
ODDTRADE DS    PL6                 TRADE BUYS                                   
ODDLNQ   EQU   *-ODDREC                                                         
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT FOR A MEDIA FILE SUMMARY ENTRY                                *         
***********************************************************************         
         SPACE 1                                                                
MTOTD    DSECT                                                                  
MTOTAGY  DS    CL2                 AGENCY CODE                                  
MTOTSUB  DS    XL1                 SUBLINE NUMBER                               
MTOTSRB  EQU   0                   REGULAR BUYS                                 
MTOTSTB  EQU   1                   TRADE BUYS                                   
MTOTNME  DS    CL24                AGENCY NAME                                  
MTOTASE  DS    XL1                 RELATIVE NUMBER IN ACLST                     
MTOTCLR  DS    PL6                 CLEARANCES                                   
MTOTCRS  DS    PL6                 CASH RECEIPTS                                
MTOTCSH  DS    PL6                 CASHPACK TOTAL                               
MTOTAMT  DS    PL6                 FILE AMOUNT                                  
MTOTDIF  DS    PL6                 DIFFERENCE                                   
MTOTCOM  DS    PL6                 COMMISSION                                   
MTOTTRD  DS    PL6                 TRADE                                        
MTOTN    EQU   (*-MTOTCLR)/L'MTOTCLR                                            
MTOTLNQ  EQU   *-MTOTD                                                          
         SPACE 2                                                                
***********************************************************************         
* DSECT FOR ACCFILE/MEDIA SUMMARY ENTRY                               *         
***********************************************************************         
         SPACE 1                                                                
SUMD     DSECT                                                                  
SUMNME   DS    CL7                 MEDIA SYSTEM NAME                            
SUMTOT   DS    PL6                 MEDIA TOTAL                                  
SUMACC   DS    (MXACC)PL6          ACC FILE TOTAL                               
SUMLNQ   EQU   *-SUMD                                                           
         EJECT                                                                  
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDCNTRL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* PAGYREC                                                                       
PAGYD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PAGYREC                                                        
         PRINT ON                                                               
* SPGENAGY                                                                      
SAGYD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPCK02S06/18/99'                                      
         END                                                                    
