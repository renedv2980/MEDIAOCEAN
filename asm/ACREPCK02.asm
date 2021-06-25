*          DATA SET ACREPCK02  AT LEVEL 038 AS OF 08/21/20                      
*PHASE ACCK02C                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE ACSMFBAL                                                               
*                                                                               
***********************************************************************         
*        HISTORY                                                      *         
***********************************************************************         
* ID   LVL DATE    JIRA         DESCRIPTION                           *         
* **** *** ******* ************ ***************************************         
* CPAT 037 31Jan19 <SPEC-31830> add functionality to override e-mail  *         
*                               address for CK151                     *         
* RGUP 038 21Aug20 <ITMF-48850> ignore sending email for printov file *         
***********************************************************************         
*                                                                               
         TITLE 'CLEARANCE CHECK REPORT'                                         
ACCK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACCK02*,R9                                                    
         L     RC,0(,R1)                                                        
         USING ACWORKD,RC                                                       
         LA    RA,SPACEND                                                       
         USING ACCKD,RA                                                         
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
         L     RF,=V(PRNTBL)                                                    
         ST    RF,APRNTBL                                                       
         MVI   MSTRT,0                                                          
         XC    LFILE,LFILE                                                      
         EJECT                                                                  
***********************************************************************         
* BUILD A LIST OF ALL ACC SE NUMBERS AND FILE CODES                   *         
***********************************************************************         
         USING CTWREC,R2           READ CONTROL FILE                            
         LA    R2,DKEY                                                          
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVI   CTWKSYSN,CTWKACC                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,DKEY,AIO                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T READ SYSTEM LIST RECORD                
*                                                                               
         USING ACLSTD,R3                                                        
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
         MVC   ACLSE#,SYSSEN       SAVE ACCOUNT SE NUMBERS                      
         MVC   ACLNAME,SYSNAME+3   SAVE ACC##                                   
         TM    ACLNAME,X'F0'       SORT NUMERIC FIRST                           
         BNO   *+8                                                              
         NI    ACLNAME,X'7F'                                                    
         LA    R3,ACLSTLNQ(,R3)                                                 
         XC    0(ACLSTLNQ,R3),0(R3)                                             
         LA    RF,1(RF)            COUNT ENTRIES                                
         STC   RF,ACC#             NUMBER OF ACC FILES                          
         CHI   RF,MXACC            TOO MANY ACC FILES                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACCK5    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     ACCK3                                                            
         EJECT                                                                  
***********************************************************************         
* HANDLE SORTING BY ACCFILE #                                                   
***********************************************************************         
ACCK7    GOTO1 XSORT,DMCB,(0,ACLST),(RF),4,2,1                                  
         LA    R3,ACLST            ASSIGN RELATIVE NUMBER TO ACC SE'S           
         IC    R0,ACC#                                                          
         SR    RF,RF                                                            
*                                                                               
ACCK9    OI    ACLNAME,X'80'       TURN BIT BACK ON                             
         STC   RF,ACLINDEX                                                      
         LA    RF,1(RF)                                                         
         LA    R3,ACLSTLNQ(,R3)                                                 
         BCT   R0,ACCK9                                                         
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ AND PROCESS ODDS FILE RECORDS                                  *         
***********************************************************************         
ACCK13   BAS   RE,INDX             GET INDEX RECORD                             
         BNE   ACCK25              END OF INDEX RECORDS - DO SUMMARY            
         MVI   OPNSW,C'N'                                                       
*                                                                               
ACCK17   GOTO1 WORKER,DMCB,WKREAD,ABUFF,ID,ODDIO                                
         CLI   8(R1),0             TEST EOF                                     
         BNE   ACCK23                                                           
*                                                                               
         USING ODDD,R5                                                          
         LA    R5,ODDIO            IGNORE ZERO RECORDS                          
*&&DO                                                                           
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
*&&                                                                             
ACCK18   CLI   OPNSW,C'Y'                                                       
         BE    *+12                                                             
         BAS   RE,OPNM             OPEN MEDIA FILES/POST AGENCY TOTALS          
         MVI   OPNSW,C'Y'                                                       
                                                                                
         USING MTOTD,R6                                                         
         LA    R6,MTOTREC                                                       
                                                                                
         USING ODDD,R5                                                          
         LA    R5,ODDIO                                                         
                                                                                
         USING ACLSTD,R3                                                        
         LA    R3,ACLST            GET RELATIVE ACC SE NUMBER                   
         LLC   R0,ACC#                                                          
         CLC   ODDSE,ACLSE#                                                     
         BE    *+14                                                             
         LA    R3,ACLSTLNQ(R3)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                INVALID ACC SE                               
*                                                                               
         MVC   MTOTASE,ACLINDEX    ACC RELATIVE NUMBER                          
         MVC   MTOTAGY,ODDAGY      AGENCY ALPHA                                 
         MVI   MTOTSUB,MTOTSRB     SET REGUALR BUY SUBLINE                      
         MVC   MTOTNME,ODDNAM      NAME                                         
         MVC   MTOTCLR(PZLQ),PZ    INITIALIZE ACCUMS                            
         ZAP   MTOTCLR,ODDAMT      CLEARANCE AMOUNT                             
         SP    MTOTCLR,ODDCR       LESS CASH RECEIPTS                           
         ZAP   MTOTCRS,ODDCR       CASH RECEIPTS                                
         ZAP   MTOTCSH,ODDAMT      CASHPACK TOTAL                               
         DROP  R3                                                               
                                                                                
         CLI   ODDLEN+1,48         TEST COMMISSION ONLY                         
         BNH   *+10                                                             
         ZAP   MTOTCOM,ODDCOMO     COMMISSION ONLY                              
         CLI   ODDLEN+1,ODDLNQ     TEST TRADE BUY BUCKET                        
         BL    ACCK20                                                           
         OC    ODDTRADE,ODDTRADE                                                
         BZ    ACCK20                                                           
         ZAP   MTOTTRD,ODDTRADE    TRADE BUYS                                   
ACCK20   BAS   RE,ADDMS            ADD TO MEDIA SUMMARY                         
*                                                                               
         CP    MTOTTRD,=P'0'       ANY TRADE BUYS                               
         BE    ACCK17                                                           
         TM    ODDSTAT,ODDSSL      Does line contain SLUSH?                     
         BNO   ACCK22                                                           
         MVI   MTOTSUB,MTOTSSA         SET SLUSH BUY SUBLINE                    
         MVC   MTOTNME,SPACES                                                   
         MVC   MTOTNME+1(14),=C'* SLUSH AMTS *'                                 
         ZAP   DUB,MTOTSLSH                                                     
         MVC   MTOTCLR(PZLQ),PZ    INITIALIZE ACCUMS                            
         AP    MTOTCLR,DUB         ADD TO CLEARANCE                             
         AP    MTOTCSH,DUB                                                      
         AP    MTOTAMT,DUB         AND FILE                                     
         BAS   RE,ADDMS            ADD IT TO SUMMARIES                          
         B     ACCK17                                                           
*                                                                               
ACCK22   MVI   MTOTSUB,MTOTSTB         SET TRADE BUY SUBLINE                    
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
         AHI   RF,SUMLNQ                                                        
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
*                                                                               
LST      USING UKRECD,R3                                                        
         MVC   0(L'ID,R3),UKRECD   SAVE THE KEY                                 
         MVC   SYSID2,UKEXTRA      SWAP THE 2ND CHAR OF SYSID                   
         MVC   LST.UKEXTRA,UKDAY                                                
         MVC   LST.UKDAY,SYSID2                                                 
         OI    LST.UKDAY,X'40'         BLANK OR CHARACTER                       
         TM    LST.UKSUBPRG,X'F0' SORT NUMERIC FIRST                            
         BNO   *+8                                                              
         NI    LST.UKSUBPRG,X'7F'                                               
         LA    R3,L'ID(R3)                                                      
         MVI   0(R3),X'FF'                                                      
         AHI   R0,1                                                             
         B     INDX3                                                            
         DROP  LST                                                              
*                                                                               
INDX5    LTR   R0,R0               TEST ANY ID'S                                
         BNZ   INDX6                                                            
*                                                                               
* don't abend if the jobs run in TST or FQA environment                         
*                                                                               
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         L     RF,MCSSB                                                         
         USING SSBD,RF                                                          
*                                                                               
         CLI   SSODSPAC,C'T'       IS THIS THE TEST SYSTEMS?                    
         JE    INDX8               YES, IGNORE THE ERROR                        
*                                                                               
         CLI   SSODSPAC,C'Q'       IS THIS THE FQA SYSTEMS?                     
         JE    INDX8               YES, IGNORE THE ERROR                        
*                                                                               
         J     *+2                 NO, MUST DUMP                                
         DROP  RE,RF                                                            
*                                                                               
INDX6    LA    RF,L'ID                                                          
         LA    R2,L'UKSYSPRG+L'UKSUBPRG+L'UKEXTRA                               
         LA    R3,UKSYSPRG-UKRECD                                               
         GOTO1 XSORT,DMCB,(0,INDXLST),(R0),(RF),(R2),(R3)                       
         LA    R3,INDXLST-L'ID                                                  
*                                                                               
INDX7    LA    R3,L'ID(R3)         R3 TO NEXT ENTRY                             
         ST    R3,AINDX                                                         
         CLI   0(R3),X'FF'         EOT                                          
         BNE   INDX10                                                           
*                                                                               
INDX8    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
         USING UKRECD,R3                                                        
INDX10   OI    UKSUBPRG,X'80'      RESET BIT THAT WE TURNED OFF                 
         MVC   SYSID2,UKDAY        SWAP OUT                                     
         MVC   UKDAY,UKEXTRA                                                    
         MVC   UKEXTRA,SYSID2                                                   
         CLI   UKEXTRA,C' '                                                     
         BH    *+8                                                              
         MVI   UKEXTRA,0           SET TO NULL IF BLANK                         
         DROP  R3                                                               
*                                                                               
         MVC   ID,0(R3)                                                         
         GOTO1 WORKER,DMCB,WKINDX,ABUFF,ID,ODDIO                                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING UKRECD,RF                                                        
         LA    RF,ID                                                            
         MVC   MSYS(MSYSLNQ),PRTF  SET PRINT FILE ENTRY                         
         CLI   UKSYSPRG,C'P'       TEST PRINT                                   
         BE    *+10                                                             
         MVC   MSYS(MSYSLNQ),SPTF  SET SPOT FILE ENTRY                          
         MVC   MSYSNUM(1),UKSUBPRG    (IE SPOT 1)                               
         MVC   MSYSNUM+1(1),UKEXTRA   ( IF TWO CHARACTER SYSTEM)                
         OC    MSYSNUM,SPACES                                                   
         CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OPEN MEDIA FILES - POST MEDIA TOTAL                                 *         
***********************************************************************         
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
         MVC   MSG,=CL20'PRINT HEADER REC'                                      
         SR    RF,RF                                                            
         ICM   RF,3,PAGYLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R2),(RF)                                        
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
GETMP5   DS    0H                                                               
         MVC   MSG,=CL20'DKEY'                                                  
         GOTO1 ADUMP,DMCB,(RC),DKEY,L'DKEY                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,MSYSDIR,DKEY,DIR                             
         CLI   8(R1),0             EOF                                          
         BNE   XIT                                                              
GETMP07  MVC   MSG,=CL20'DIR'                                                   
         GOTO1 ADUMP,DMCB,(RC),DIR,L'DIR                                        
         MVC   DKEY,DIR            SAVE KEY FOR NEXT                            
         CLI   PAGYKRCD,1          MAKE SURE REC TYPE IS 1                      
         BE    GETMP10             ELSE FORCE M=I REC=1                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,MSYSDIR,DKEY,DIR                             
         CLI   8(R1),0                                                          
         BNE   XIT                                                              
         B     GETMP07                                                          
*                                                                               
GETMP10  MVC   DA,DIR+27                                                        
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
ADDSY    NTR1  ,                                                                
         USING MTOTD,R6                                                         
         LA    R6,MTOTREC          R6=CURRENT MEDIA ENTRY                       
                                                                                
         USING SUMD,R7                                                          
         L     R7,ANXTSUM          ADD TO SUMMARY ENTRY                         
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
         MHI   R1,L'SUMACC                                                      
         LA    RF,SUMACC(R1)                                                    
         AP    0(6,RF),MTOTCSH     ADD CASHPACK TOTAL                           
         AP    SUMTOT,MTOTCSH                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT MEDIA REPORT                                                  *         
***********************************************************************         
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
         MVC   EFILE(L'MSYSNME),MSYSNME                                         
*                                                                               
REPT3    MVC   P+1(2),MTOTAGY                                                   
         CLI   MTOTSUB,MTOTSRB                                                  
         BE    *+10                                                             
         MVC   P+1(2),SPACES                                                    
         MVC   P+4(L'MTOTNME),MTOTNME                                           
         ZAP   MTOTDIF,MTOTAMT     FILE AMOUNT                                  
         SP    MTOTDIF,MTOTCSH     LESS CASHPAK = DIFFERENCE                    
*                                                                               
         LA    R0,MTOTN                                                         
         LA    R1,MTOTCLR          ADD ENTRY TO TOTAL                           
         LA    R2,MTOTCLR-MTOTD(R5)                                             
         SR    RF,RF                                                            
*                                                                               
         MVC   EDETAIL,SPACES      CLEAR FOR NEW DETAILS                        
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
         CP    MTOTDIF,=P'0'       IF NO DIFFERENCE, DON'T PRINT DETAIL         
         BE    REPT7                                                            
*                                                                               
         USING MASTD,RE                                                         
         L     RE,ADMASTC                                                       
         TM    MCFLAGS2,MCEMAIL    DOES EMAIL = Y ?                             
         JZ    REPT7               YES                                          
         DROP  RE                                                               
*                                                                               
         CLI   MSYSNME,C'P'        IS THIS PRINT?                               
         BNE   REPT6               NO                                           
         CLC   MSYSNUM,=C'TU'      SKIP, PRINT TU AND TT FILES                  
         BE    REPT7                                                            
         CLC   MSYSNUM,=C'TT'                                                   
         BE    REPT7                                                            
         CLC   MSYSNUM,=C'OV'      Overnight file on FQA                        
         BE    REPT7                                                            
*                                                                               
REPT6    BAS   RE,EDEM             EDIT EMAIL                                   
         BAS   RE,SENDMAIL                                                      
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
*                                                                               
         CP    MTOTDIF,=P'0'       ONLY SEND EMAIL IF OOB                       
         BNE   REPT9                                                            
REPT8    CLI   MSTRT,0                                                          
         BE    REPTX                                                            
         GOTOR VSMTP,DMCB,('SMTPCNCL',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
         MVI   MSTRT,0             START OVER                                   
         B     REPTX                                                            
*                                                                               
REPT9    DS    0H                                                               
         CLI   MSYSNME,C'P'        IS THIS PRINT?                               
         BNE   REPT10              NO                                           
         CP    MTOTDIF,=P'100'     MORE THAN A DOLLAR?                          
         BH    REPT10              YES, PRINT IT                                
         CP    MTOTDIF,=P'-100'                                                 
         BH    REPT8                                                            
*                                                                               
REPT10   BAS   RE,EDET             EDIT MEDIA TOTAL                             
         CLI   MSTRT,0                                                          
         BE    REPTX                                                            
         GOTOR VSMTP,DMCB,('SMTPAPTL',EDETAIL)                                  
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
REPT11   MVI   MSTRT,0             START OVER                                   
*                                                                               
REPTX    B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* EDIT MEDIA TOTALS                                                 *           
*********************************************************************           
EDME     NTR1  ,                                                                
         LA    R0,6                                                             
         LA    R2,P+30                                                          
         LA    R3,MTOTCLR                                                       
EDME3    EDIT  (P6,0(R3)),(14,0(R2)),2,ZERO=BLANK,MINUS=YES                     
         LA    R2,17(R2)                                                        
         LA    R3,6(R3)                                                         
         BCT   R0,EDME3                                                         
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* EDIT EMAIL DETAILS                                                *           
*********************************************************************           
EDEM     NTR1  ,                                                                
         MVC   EDETAIL,SPACES                                                   
         LA    R0,5                                                             
         LA    R2,ECLR                                                          
         LA    R3,MTOTCLR                                                       
EDEM3    EDIT  (P6,0(R3)),(14,0(R2)),2,ZERO=BLANK,MINUS=YES                     
         LA    R2,15(R2)                                                        
         LA    R3,6(R3)                                                         
         BCT   R0,EDEM3                                                         
         MVC   EAGY,MTOTAGY                                                     
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* EDIT EMAIL TOTALS                                                 *           
*********************************************************************           
EDET     NTR1  ,                                                                
         MVC   EDETAIL,SPACES                                                   
         MVC   EAGY,=C'**'                                                      
         LA    R0,5                                                             
         LA    R2,ECLR                                                          
         LA    R3,MTOTCLR                                                       
EDET3    EDIT  (P6,0(R3)),(14,0(R2)),2,ZERO=BLANK,MINUS=YES                     
         LA    R2,15(R2)                                                        
         LA    R3,6(R3)                                                         
         BCT   R0,EDET3                                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EMAIL ERROR REPORT                                                 *          
**********************************************************************          
SENDMAIL NTR1                                                                   
         CLI   MSTRT,0             IS THIS THE FIRST CALL?                      
         BNE   SENDM08             NO                                           
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         MVI   MSTRT,1                                                          
         MVC   SUBJOBN(L'RCJOB),RCJOB                                           
*                                                                               
SENDM02  MVC   TOWHO,PRNTWHO                                                    
         CLI   MSYSNME,C'P'        SEND EMAIL TO PRINT                          
         BE    SENDM06                                                          
         MVC   TOWHO,SPOTWHO                                                    
         MVC   SYSALPHA,MSYSNME                                                 
         MVC   SYSLETTR,MSYSNUM                                                 
         GOTO1 DATAMGR,DMCB,(0,DDNAME),SYSINFO,0                                
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    *+2                                                              
         USING DDNAMED,RE                                                       
         CLI   DDNASYN3,C'N'                                                    
         BNE   SENDM06                                                          
         MVC   TOWHO,NETWHO                                                     
         DROP  RE                                                               
*                                                                               
         USING MASTD,RE                                                         
SENDM06  L     RE,ADMASTC                                                       
         L     RE,MCAEXTRA                                                      
         USING MCEXTRA,RE                                                       
         CLC   MC@EMAIL,SPACES          DID WE PASS EID = EMAIL ADDRESS         
         JNH   SENDM07                  NO -SEND EMAIL TO EXISTING USER         
*                                                                               
         MVC   TOWHO,SPACES             YES -OVERRIDE EMAIL ADDRESSES           
         MVC   TOWHO,MC@EMAIL           WITH PASSED EID                         
         DROP  RE                                                               
*                                                                               
SENDM07  GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO),(L'SUBDESC,SUBDESC)                
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEAD)                                    
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEADU)                                   
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEAD2)                                   
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEADU2)                                  
*                                                                               
SENDM08  GOTOR VSMTP,DMCB,('SMTPAPTL',EDETAIL)                                  
         MVC   LTOWHO,TOWHO                                                     
         MVC   LFILE,EFILE                                                      
*                                                                               
SENDMX   B     XIT                                                              
                                                                                
DDNAME   DC    CL8'DDNAME'                                                      
SYSINFO  DC    C'S='                                                            
SYSALPHA DC    C' '                                                             
SYSLETTR DC    C'  '                                                            
*                                                                               
SYSSE    DC    C'SE=',X'0000'                                                   
         ORG   SYSSE+3                                                          
         DS    X                                                                
SYSSE#   DS    X                                                                
         EJECT                                                                  
*********************************************************************           
* GET OVERALL TOTALS                                                *           
*********************************************************************           
ALLS     NTR1  ,                                                                
         USING SUMD,R7                                                          
         L     R7,ANXTSUM          INITIALIZE SUMMARY OVERALL TOTAL             
         MVC   SUMNME,=C'*TOTAL* ' SYSTEM NAME                                  
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
         AHI   R5,SUMLNQ                                                        
         CR    R5,R7               R5 = THE TOTAL LINE                          
         BL    ALLS5                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT SUMMARY                                                       *         
***********************************************************************         
PSUM     NTR1  ,                                                                
         MVI   RCSUBPRG,1                                                       
         SR    R1,R1                                                            
         IC    R1,ACC#             NUMBER OF ACC FILE                           
         SR    R0,R0                                                            
         D     R0,=F'6'                                                         
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R1,1                                                             
         STC   R1,NUMPAGE          R0=NUMBER OF SUMMARY PAGES                   
         MVI   PAGENUM,1           PAGE (PASS NUMBER)                           
         LA    R4,1                                                             
*                                                                               
         USING SUMD,R7                                                          
PSUM3    L     R7,ASTOTS           SET HEADLINES                                
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING ACLSTD,R3                                                        
PSUM5    SR    R4,R4                                                            
         IC    R4,PAGENUM          6 ACC FILES PER PAGE                         
         BCTR  R4,0                                                             
         LR    R3,R4                                                            
         MHI   R3,ACLSTLNQ*6                                                    
         LA    R3,ACLST(R3)        R3=ACLST ENTRY                               
         MHI   R4,L'SUMACC*6                                                    
         LA    R4,SUMACC(R4)       R4=ACCUMS                                    
         L     RF,ACREPORT                                                      
         CLI   SUMNME,C'*'         TEST TOTAL LINE                              
         BNE   *+6                                                              
         BASR  RE,RF               SKIP A LINE                                  
         MVC   P+1(8),SUMNME       MEDIA SYSTEM NAME                            
         EDIT  (P7,SUMTOT),(15,P+14),2,ZERO=BLANK,MINUS=YES                     
*                                                                               
         LA    R0,6                R0=COLUMN COUNT                              
         LA    R2,P+31             R2=PRINT LINE POSITION                       
         LA    R1,HEAD5+31         R1=HEADLINE POSITION                         
*                                                                               
PSUM9    MVC   4(3,R1),=C'ACC'                                                  
         MVC   9(2,R1),ACLNAME     ACC NUMBER                                   
         MVC   L'P(14,R1),=14C'-'                                               
         EDIT  (P6,0(R4)),(14,0(R2)),2,ZERO=BLANK,MINUS=YES                     
*                                                                               
         USING UTLD,RE                                                          
         CLI   SUMNME,C'*'                                                      
         BNE   PSUM10              ONLY WANTED *TOTAL*                          
         L     RE,AUTL                                                          
         ST    R1,SVR1             NEED TO SAVE R1 SINCE HEADING                
         ZAP   TOTP8,0(6,R4)                                                    
         LLC   RF,ACLSE#           Over-ride SE#                                
         GOTOR =V(ACSMFBAL),DMCB,(1,(RC)),TOTP8,PZERO,PZERO,0,(RF)              
         CLI   4(R1),1                                                          
         BNE   *+6                                                              
         DC    H'00'               SMFOUT missing                               
         L     R1,SVR1                                                          
         DROP  RE                                                               
*                                                                               
PSUM10   LA    R1,17(,R1)          ACCOUNT HEADING                              
         LA    R2,17(,R2)          PRINT LINE                                   
         LA    R3,ACLSTLNQ(,R3)    ACCOUNT FILE LIST                            
         LA    R4,6(,R4)           AMOUNT                                       
         CLI   0(R3),0             TEST END OF LIST                             
         BE    *+8                                                              
         BCT   R0,PSUM9                                                         
*                                                                               
         GOTO1 ACREPORT                                                         
         CLI   SUMNME,C'*'         TEST TOTAL LINE                              
         BE    *+12                                                             
         AHI   R7,SUMLNQ                                                        
         B     PSUM5                                                            
         DROP  R3                                                               
*                                                                               
         CLC   PAGENUM,NUMPAGE     FINISHED LAST PAGE                           
         BE    XIT                                                              
         SR    R4,R4                                                            
         IC    R4,PAGENUM                                                       
         AHI   R4,1                NEXT PAGE                                    
         STC   R4,PAGENUM                                                       
         B     PSUM3               NEXT SET OF ACC FILES                        
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
MXACC    EQU   40                  MAXIMUM ACCOUNT FILES                        
MXMED    EQU   40                  MAXIMUM MEDIA FILES                          
MXAGY    EQU   20                  MAXIMUM AGENCIES ON MEDIA FILE               
*                                                                               
DATVAL   DC    V(DATVAL)                                                        
ADUMP    DC    A(DUMP)                                                          
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
SPTF     DC    CL8'SPOT  ??'                                                    
         DC    CL8'SPOT'                                                        
         DC    CL8'SPTDIR'                                                      
         DC    CL8'SPTFIL'                                                      
         DC    CL17'NSPTDIR NSPTFIL X'                                          
*                                                                               
PRTF     DC    CL8'PRINT ??'                                                    
         DC    CL8'PRINT'                                                       
         DC    CL8'PRTDIR'                                                      
         DC    CL8'PRTFILE'                                                     
         DC    CL17'NPRTDIR NPRTFILEX'                                          
*                                                                               
PZ       DC    (MTOTN)PL6'0'                                                    
PZLQ     EQU   *-PZ                                                             
PZERO    DC    PL8'0'              Leave as PL8, ACSMFBAL needs it              
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
*                                                                               
TOWHO    DS    CL48                                                             
SPOTWHO  DC    CL48'NA-OOB_SPOT_TEAM,NA-OOB_TEAM:'                              
PRNTWHO  DC    CL48'NA-OOB_PRINT_TEAM,NA-OOB_TEAM:'                             
NETWHO   DC    CL48'NA-OOB_NET_TEAM,NA-OOB_TEAM:'                               
                                                                                
SUBDESC  DC    0CL80                                                            
SUBJOBN  DS    CL(L'RCJOB)                                                      
         DC    CL72' - 151 CHECK CLEARANCE DIFFERENCES'                         
*                                                                               
*UBDESC  DC    CL80'151 TESTING TESTING TESTING    '                            
                                                                                
*                                                                               
EHEAD    DS    0CL80                                                            
EFILE    DC    CL8' '                                                           
         DS    CL(L'EHEAD-(*-EHEAD))' '                                         
*                                                                               
EHEAD2   DS    0CL80                                                            
         DC    C' '                                                             
         DC    CL2'AG'                                                          
         DC    C' '                                                             
         DC    CL14'  CLEARANCES  '                                             
         DC    C' '                                                             
         DC    CL14' CASH RECEIPTS'                                             
         DC    C' '                                                             
         DC    CL14' CASHPAK TOTAL'                                             
         DC    C' '                                                             
         DC    CL14' FILE AMOUNT  '                                             
         DC    C' '                                                             
         DC    CL14' DIFFERENCE    '                                            
         DS    CL(L'EHEAD2-(*-EHEAD2))' '                                       
*                                                                               
EHEADU   DS    0CL80                                                            
         DC    CL7'-------'                                                     
         DC    CL(L'EHEADU-(*-EHEADU))' '                                       
*                                                                               
EHEADU2  DS    0CL80                                                            
         DC    C' '                                                             
         DC    CL2'--'                                                          
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    CL(L'EHEADU2-(*-EHEADU2))' '                                     
*                                                                               
EDETAIL  DS    0CL80                                                            
         DS    C                                                                
EAGY     DS    CL2                                                              
         DS    C                                                                
ECLR     DS    CL14                                                             
         DS    C                                                                
ECRS     DS    CL14                                                             
         DS    C                                                                
ECSH     DS    CL14                                                             
         DS    C                                                                
EAMT     DS    CL14                                                             
         DS    C                                                                
EDIF     DS    CL14                                                             
         DS    CL(L'EDETAIL-(*-EDETAIL))                                        
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS/IO AREAS/                                                   *         
***********************************************************************         
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
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(,R1)                                                        
         L     R4,8(,R1)                                                        
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 APRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),        *        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
         DROP  RB                                                               
*        MVC   MSG,=CL20'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* LOCAL WORKING STORGAE                                              *          
**********************************************************************          
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
*                                                                               
ACCKD    DSECT                                                                  
TODAY    DS    XL3                                                              
ID       DS    CL16                ODDFILE ID                                   
AUTL     DS    A                   A(UTL)                                       
APRNTBL  DS    A                   ADDRESS OF PRINTABLE RTE                     
SVR1     DS    A                   SAVE R1                                      
DKEY     DS    CL64                KEY FOR DIRECTORY READ                       
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    XL4                 DISK ADDRESS                                 
ACLST    DS    (MXACC)XL(ACLSTLNQ) ACC SE/ACC FILE #/RELATIVE #                 
ACC#     DS    X                   NUMBER OF ACCFILES                           
SVUTLSE# DS    X                   SAVE UTL SE# (TSYS)                          
TOTP8    DS    PL8                 TOTAL PACKED 8                               
LFILE    DS    CL7                 LAST FILE                                    
LTOWHO   DS    CL10                LAST TOWHO (10 BYTES ONLY)                   
*                                                                               
SYSID2   DS    CL1                 2ND CHARACTER OF SYSTEM ID                   
*                                                                               
MSG      DS    CL20                                                             
MSYS     DS    0C                                                               
MSYSNME  DS    CL8                 MEDIA SYSTEM NAME / NUMBER                   
         ORG   MSYSNME+L'MSYSNME-2                                              
MSYSNUM  DS    CL2                 NUMBER                                       
MSYSTEM  DS    CL8                 SYSTEM                                       
MSYSDIR  DS    CL8                 DIRECTORY NAME                               
MSYSFIL  DS    CL8                 FILE NAME                                    
MSYSOPN  DS    CL17                OPEN LIST                                    
MSYSLNQ  EQU   *-MSYS                                                           
*                                                                               
ODDIO    DS    CL(ODDLNQ)          ODDFILE RECORD                               
MTOTREC  DS    CL(MTOTLNQ)         MEDIA SUMMARY RECORD                         
ANXTSUM  DS    A                   A(NEXT SUMMARY ENTRY)                        
*                                                                               
PAGENUM  DS    X                   SUMMARY PAGE NUMBER                          
NUMPAGE  DS    X                   NUMBER OF SUMMARY PAGES REQUIRED             
OPNSW    DS    CL1                 Y=MEDIA FILE HAS BEEN OPENED                 
*                                                                               
INDXLST  DS    (MXMED)XL(L'ID)     INDEX RECORD LIST                            
*                                                                               
MSTRT    DS    X                   0=FIRST CALL TO SENDMAIL                     
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER AN ODDFILE RECORD                                    *         
***********************************************************************         
ODDD     DSECT                                                                  
ODDREC   DS    0C                                                               
ODDLEN   DS    XL4                                                              
ODDACC   DS    CL1                 ACCOUNTING AGENCY CODE                       
ODDID    DS    CL2                 CONTL AGENCY ID                              
ODDUTL   DS    CL1                 UTL NUMBER                                   
ODDAGY   DS    CL2                 SPOT OR PRINT AGENCY CODE                    
ODDAMT   DS    PL6                 AMOUNT FROM CLEARANCE AC54                   
ODDNAM   DS    CL23                AGENCY NAME                                  
ODDSTAT  DS    XL1                 STATUS BYTE                                  
ODDSSL   EQU   X'80'               LINE HAS A SLUSH AND NOT TRADE               
ODDCR    DS    PL5                 CASH RECEIPT                                 
ODDSE    DS    XL1                 ACCOUNT SE NUMBER                            
ODDCOMO  DS    PL6                 COMMISSION ONLY                              
ODDTRADE DS    PL6                 TRADE BUYS                                   
         ORG   ODDTRADE                                                         
ODDSLUSH DS    PL6                 SLUSH AMOUNT                                 
ODDLNQ   EQU   *-ODDREC                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR A MEDIA FILE SUMMARY ENTRY                                *         
***********************************************************************         
MTOTD    DSECT                                                                  
MTOTAGY  DS    CL2                 AGENCY CODE                                  
MTOTSUB  DS    XL1                 SUBLINE NUMBER                               
MTOTSRB  EQU   0                   REGULAR BUYS                                 
MTOTSTB  EQU   1                   TRADE BUYS                                   
MTOTSSA  EQU   2                   SLUSH AMOUNTS                                
MTOTNME  DS    CL23                AGENCY NAME                                  
MTOTSTAT DS    XL1                 STATUS BYTE                                  
MTOTASE  DS    XL1                 RELATIVE NUMBER IN ACLST                     
MTOTCLR  DS    PL6                 CLEARANCES                                   
MTOTCRS  DS    PL6                 CASH RECEIPTS                                
MTOTCSH  DS    PL6                 CASHPACK TOTAL                               
MTOTAMT  DS    PL6                 FILE AMOUNT                                  
MTOTDIF  DS    PL6                 DIFFERENCE                                   
MTOTCOM  DS    PL6                 COMMISSION                                   
MTOTTRD  DS    PL6                 TRADE                                        
         ORG   MTOTTRD                                                          
MTOTSLSH DS    PL6                 SLUSH                                        
MTOTN    EQU   (*-MTOTCLR)/L'MTOTCLR                                            
MTOTLNQ  EQU   *-MTOTD                                                          
***********************************************************************         
* DSECT FOR ACCFILE/MEDIA SUMMARY ENTRY                               *         
***********************************************************************         
SUMD     DSECT                                                                  
SUMNME   DS    CL8                 MEDIA SYSTEM NAME                            
SUMTOT   DS    PL7                 MEDIA TOTAL                                  
SUMACC   DS    (MXACC)PL6          ACC FILE TOTAL                               
SUMLNQ   EQU   *-SUMD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ACLST SINGLE ENTRY                                        *         
***********************************************************************         
ACLSTD   DSECT                                                                  
ACLSE#   DS    X                   ACC SE#                                      
ACLNAME  DS    CL2                 ACCOUNT FILE                                 
ACLINDEX DS    X                   ACCOUNT FILE INDEX                           
ACLSTLNQ EQU   *-ACLSTD                                                         
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
* DMDDAMED                                                                      
DDNAMED  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DMDDNAMED                                                      
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
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
                                                                                
* FASSBOFF                                                                      
SSBD     DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
                                                                                
* DMSPTNETD                                                                     
*        PRINT OFF                                                              
*      ++INCLUDE DMSPTNETD                                                      
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038ACREPCK02 08/21/20'                                      
         END                                                                    
