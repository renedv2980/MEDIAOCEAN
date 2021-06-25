*          DATA SET FATABSGTR  AT LEVEL 002 AS OF 10/30/18                      
*PHASE TABSGTRA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DDINFO                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATTIM                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE BINSR31                                                                
*                                                                               
         TITLE 'FATABSGTR - BUILD TABS DATASPACE TABLE FOR GETRET'              
*                                                                               
TABSGTR  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         ENTRY COMFACS                                                          
*                                                                               
         PRINT NOGEN                                                            
         NBASE WORKDLQ,*TABSGTR,=V(REGSAVE)                                     
         USING WORKD,RC                                                         
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BRAS  RE,INIT                                                          
         JNE   NEQXIT                                                           
*                                                                               
         BRAS  RE,MAIN                                                          
*                                                                               
         XBASE                                                                  
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
$$DATA   LOCTR ,                   DATA LOCATION CLOSE TO RB                    
*                                                                               
$$CODE   LOCTR ,                   CODE AFTER DATA                              
MAIN     NTR1                                                                   
         BRAS  RE,READHOL          READ HOLIDAY DEFINITION DATASET              
*                                                                               
* BUILD A COPY OF HOLIDAY TABLES IN 31-BIT STORAGE                              
*                                                                               
         MVC   P(30),=CL30'HOLIDAY TABLE'                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=30C'-'                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     R3,AHOLTAB          BINSRCH SORTED TABLE OF HOLIDAYS             
         USING HOLTABD,R3                                                       
         ICM   R5,15,BP3           NUMBER OF RECORDS IN TABLE                   
         JZ    ERTABEMP            TABLE EMPTY: SOMETHING WENT WRONG            
*                                                                               
         MVI   BYTE,0              LAST ENTRY'S COUNTRY CODE                    
         MVI   BYTE2,0             ENTRIES PER COUNTRY                          
*                                                                               
MAIN50   DS    0H                                                               
         SAM31                                                                  
         CLC   BYTE,0(R3)          HAS COUNTRY CODE CHANGED?                    
         SAM24                                                                  
         JE    MAIN70                                                           
*                                                                               
* YES, NEW COUNTRY CODE - FIND COUNTRY NAME AND BUILD TABLE HEADER              
*                                                                               
         SAM31                                                                  
         MVC   BYTE,0(R3)          SAVE COUNTRY CODE                            
         SAM24                                                                  
         MVI   BYTE2,0             RE-SET ENTRIES PER COUNTRY                   
*                                                                               
* TALENT TABLE NAMES ARE HARD-CODED                                             
* POINT R4 TO TABLE NAME                                                        
*                                                                               
         CLI   BYTE,CTRYTALQ       TALENT CALENDAR?                             
         JNE   *+12                                                             
         LA    R4,=CL11'TALENT'                                                 
         J     MAIN60                                                           
*                                                                               
         CLI   BYTE,CTRYTPUQ       TALENT PARTNERS CALENDAR?                    
         JNE   *+12                                                             
         LA    R4,=CL11'TAL PARTNRS'                                            
         J     MAIN60                                                           
*                                                                               
* INDEX INTO COUNTRY TABLE TO GET COUNTRY NAME                                  
*                                                                               
         LLC   RE,BYTE                                                          
         BCTR  RE,0                                                             
         MHI   RE,CTRYTABL                                                      
         LARL  RF,CTRYTAB1                                                      
         AR    RF,RE               INDEX INTO COUNTRY TABLE                     
         LA    R4,CTRYNAM-CTRYTABD(RF) POINT R4 TO COUNTRY NAME                 
*                                                                               
* INDEX INTO THE HOLIDAY TABLE IN 31-BIT STORAGE                                
*                                                                               
MAIN60   DS    0H                                                               
         LLC   RE,BYTE             COUNTRY CODE                                 
         BCTR  RE,0                                                             
         MHI   RE,HOLTYMXQ*HOLTABNQ*HOLTABDLQ+HOLTTTLQ                          
         L     R2,AHOLTAB          OBTAINED 31-BIT STORAGE                      
         A     R2,=A(CRDTABLQ)     MOVE PAST CARDS TABLE                        
         AR    R2,RE               INDEX INTO THE TABLE (BY CTRY CODE)          
*                                                                               
* COPY COUNTRY NAME FROM CTRYTAB TO GETRET DATASPACE                            
*                                                                               
         SAM31                                                                  
         MVC   0(5,R2),=5C'*'      EYCATCHER                                    
         MVC   5(L'CTRYNAM,R2),0(R4) R4 POINTS TO COUNTRY NAME                  
         SAM24                                                                  
*                                                                               
         SAM31                                                                  
         MVC   P(HOLTTTLQ),0(R2)                                                
         SAM24                                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         AHI   R2,HOLTTTLQ         ADVANCE PAST TABLE HEADER                    
*                                                                               
MAIN70   DS    0H                                                               
         AHI   R3,1 ADVANCE PAST COUNTRY CODE IN BINSRCH TABLE ENTRY            
*                                                                               
         SAM31                                                                  
         MVC   0(HOLTABDLQ,R2),0(R3)                                            
         SAM24                                                                  
*                                                                               
         LLC   R0,BYTE2            ENTRIES PER COUNTRY                          
         AHI   R0,1                                                             
         CHI   R0,HOLTYMXQ*HOLTABNQ                                             
         JNL   ERMAXENT                                                         
*                                                                               
         SAM31                                                                  
         MVC   P(L'HOLTDATE),HOLTDATE                                           
         MVC   P+L'HOLTDATE+1(L'HOLTDESC),HOLTDESC                              
         SAM24                                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         AHI   R3,HOLTABDLQ        BINSRCH TABLE POINTER                        
         AHI   R2,HOLTABDLQ        DATASPACE TABLE POINTER                      
         BRCT  R5,MAIN50                                                        
*                                                                               
* ALL DATES IN BINSRCH TABLE PROCESSED                                          
* NOW COPY THE COMPLETED HOLIDAY TABLE FROM 31-BIT STORAGE                      
* TO TABS DATASPACE                                                             
*                                                                               
* LOCK GETRET TABLE IN TABS DATASPACE                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTGETRET)                                            
         GOTO1 =V(LOCKSPC),DUB                                                  
         ICM   RF,15,4(R1)                                                      
         JZ    ERTABLOK                                                         
*                                                                               
         SAM31                                                                  
         ICM   R2,15,(DSPTFRST-DMSPACED)(RF) DISP OF GETRET TAB IN DSP          
         SAM24                                                                  
*                                                                               
         ICM   R3,15,=AL4(HOLTABLQ) LENGTH OF TABLE TO BE MOVED                 
         L     R4,AHOLTAB          OBTAINED 31-BIT STORAGE                      
         A     R4,=A(CRDTABLQ)     MOVE PAST CARDS TABLE                        
         LR    R5,R3                                                            
*                                                                               
* LOAD TABS DATASPACE ALET INTO AR2                                             
         LARL  RF,SSB                                                           
         LAM   AR2,AR2,SSBTBLET-SSBD(RF)                                        
*                                                                               
         CLI   WRITEFL,C'Y'                                                     
         JNE   MAIN80                                                           
*                                                                               
         SAM31                                                                  
         SAC   512                                                              
         MVCL  R2,R4                                                            
         SAC   0                                                                
         SAM24                                                                  
*                                                                               
MAIN80   DS    0H                                                               
         LAM   AR2,AR2,ARZERO      CLEAR ACCESS REGISTER AR2                    
*                                                                               
* UNLOCK GETRET TABLE IN DATASPACE                                              
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTGETRET)                                            
         MVI   DUB,X'10'                                                        
         GOTO1 =V(LOCKSPC),DUB                                                  
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MAINX    DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
* INITIALIZE PROGRAM                                                            
*                                                                               
INIT     NTR1                                                                   
         LAM   AR0,ARF,ARZERO      CLEAR ACCESS REGISTERS                       
*                                                                               
         LA    RE,WORKD                                                         
         ICM   RF,15,=AL4(WORKDLQ)                                              
         XCEFL                                                                  
*                                                                               
         MVI   WRITEFL,C'N'                                                     
         MVI   BACKFL,C'N'                                                      
         MVI   LOOPFL,C'N'                                                      
         MVI   HOLSTFL,C'N'                                                     
*                                                                               
* READ PARAMETER CARDS                                                          
*                                                                               
         MVC   P(13),=CL13'CONTROL CARDS'                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=13C'-'                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
INI10    DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         JE    INI300                                                           
*                                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'                                                        
         JE    INI10                                                            
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         JNE   INI20                                                            
         ICM   RF,15,=V(DDSIO)                                                  
         JZ    INI10                                                            
         MVC   0(8,RF),CARD+6                                                   
         J     INI10                                                            
*                                                                               
INI20    DS    0H                                                               
         CLC   =C'STXIT',CARD                                                   
         JNE   INI36                                                            
         CLI   CARD+6,C'Y'                                                      
         JNE   INI10                                                            
*                                                                               
         L     RF,=A(TABSGTR)                                                   
         ST    RF,WORK                                                          
         L     RF,=A(TABSGTRX)                                                  
         LA    RF,40(,RF)          SHOW LEVEL=, DATE=                           
         ST    RF,WORK+4                                                        
         OI    WORK+4,X'80'        EOL                                          
         GOTO1 =V(STXITER),DMCB,WORK                                            
         J     INI10                                                            
*                                                                               
INI36    DS    0H                                                               
         CLC   =C'DSPACE=',CARD                                                 
         JNE   INI54                                                            
*                                                                               
         LARL  RF,SSB                                                           
         CLI   SSOXTND-SSOOFF(RF),X'FF'                                         
         JNE   INI10                                                            
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         J     INI10                                                            
*                                                                               
INI54    DS    0H                                                               
         CLC   =C'WRITE=',CARD                                                  
         JNE   INI298                                                           
*                                                                               
         CLC   =C'YES',CARD+6                                                   
         JNE   INI10                                                            
         MVI   WRITEFL,C'Y'                                                     
         J     INI10                                                            
*                                                                               
INI298   DS    0H                                                               
         MVC   P(10),=CL10'BAD CARD'                                            
         GOTO1 =V(PRINTER)                                                      
         J     INITNQX                                                          
*                                                                               
INI300   DS    0H                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* DUMMY DATAMANAGER CALL TO OBTAIN TABS DATASPACE ALET                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DUMMY'                                       
*                                                                               
         ICM   R0,15,=AL4(HOLTABLQ+CRDTABLQ)                                    
         STORAGE OBTAIN,LENGTH=(R0),LOC=31,COND=YES                             
         LTR   RF,RF                                                            
         JNZ   ERNOOBT                                                          
         ST    R1,AHOLTAB                                                       
*                                                                               
* CLEAR THE OBTAINED STORAGE                                                    
*                                                                               
         L     RE,AHOLTAB                                                       
         ICM   RF,15,=AL4(HOLTABLQ+CRDTABLQ)                                    
         SAM31                                                                  
         XCEFL                                                                  
         SAM24                                                                  
*                                                                               
* SET UP BINSRCH PARAMETERS                                                     
*                                                                               
         XC    BSPARS,BSPARS                                                    
*                                                                               
         LA    RF,WORK                                                          
         ST    RF,BP1              P1: RECORD AREA                              
         L     RF,AHOLTAB                                                       
         ST    RF,BP2              P2: BUFFER                                   
*                                  P3: RECORDS IN TABLE                         
*                                  P4: RECORD LENGTH                            
         MVI   BP4+L'BP4-1,HOLTABDLQ+1 HOLTAB ENTRY + COUNTRY CODE              
*                                  P5: KEY LENGTH                               
         MVI   BP5+L'BP5-1,HOLTABDLQ+1                                          
*                                  P6: MAX ENTRIES IN TABLE                     
*                                  MAX COUNTRIES * MAX PER COUNTRY *            
         LHI   RF,HOLTABNQ*HOLTCMXQ*HOLTYMXQ       MAX YEARS PER CTRY           
         ST    RF,BP6                                                           
*                                                                               
INITQX   J     EQXIT                                                            
INITNQX  J     NEQXIT                                                           
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* READ HOLIDAY CARDS                                                  *         
***********************************************************************         
READHOL  NTR1  ,                                                                
         MVC   P(30),=CL30'HOLIDAY CARDS'                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=30C'-'                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OPEN  (HOLIDAYS,(INPUT))                                               
         LTR   RF,RF                                                            
         JNZ   ERNOOPEN                                                         
*                                                                               
         LA    R2,CARD             CONTROL CARD                                 
         USING HOLCARDD,R2                                                      
         LA    R3,WORK+1           LEAVE 1 BYTE FOR COUNTRY CODE                
         USING HOLTABD,R3                                                       
*                                                                               
RHOL10   DS    0H                                                               
         GET   HOLIDAYS,CARD       READ HOLIDAY DEFINITION CARD                 
*                                                                               
         CLC   =C'*',CARD                                                       
         JE    RHOL10                                                           
*                                                                               
         MVC   P(L'CARD),CARD                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RHOL15   DS    0H                                                               
*                                                                               
         CLC   =C'TAL',HOLCCTRY    TALENT CALENDAR ENTRY?                       
         JNE   *+12                                                             
         MVI   WORK,CTRYTALQ       "FAKE" COUNTRY CODE - SEE DDHOLTABD          
         J     RHOL30                                                           
*                                                                               
         CLC   =C'TPU',HOLCCTRY    TALENT PARTNERS?                             
         JNE   *+12                                                             
         MVI   WORK,CTRYTPUQ       "FAKE" COUNTRY CODE - SEE DDHOLTABD          
         J     RHOL30                                                           
*                                                                               
         LARL  R4,CTRYTAB1         1ST COUNTRY CODE IN FACTRYTAB (UK)           
         USING CTRYTABD,R4                                                      
*                                                                               
RHOL20   DS    0H                                                               
         CLI   CTRYCODE,X'FF'      EOT?                                         
         JE    ERBADCTR         HOLIDAY'S COUNTRY CODE NOT IN FACTRYTAB         
*                                                                               
         CLC   HOLCCTRY,CTRYSHR    3-CHAR COUNTRY CODE                          
         JE    RHOL25                                                           
         LA    R4,CTRYTABL(R4)     NEXT COUNTRY IN FACTRYTAB                    
         J     RHOL20                                                           
*                                                                               
RHOL25   DS    0H                                                               
         MVC   WORK(1),CTRYCODE    1-BYTE CTRY CODE EQU FOR BINSRCH             
         DROP  R4                  CTRYTABD                                     
*                                                                               
RHOL30   DS    0H                                                               
         GOTO1 =V(DATVAL),DMCB,(0,HOLCDATE),HOLTDATE                            
         OC    DMCB(4),DMCB        DATE STRING LENGTH                           
         JZ    ERBADDAT            INVALID DATE                                 
*                                                                               
         GOTO1 =V(GETDAY),DMCB,(0,HOLTDATE),WORK2                               
         CLI   DMCB,0              RETURNED DAY OF THE WEEK(BINARY)             
         JZ    ERGETDAY            SOMETHING WRONG WITH GETDAY                  
         LLC   RF,DMCB                                                          
         CVD   RF,DUB                                                           
         ZAP   HOLTDAY,DUB                                                      
*                                                                               
         MVC   HOLTTYPE,HOLCTYPE                                                
         MVC   HOLTDESC,HOLCDESC                                                
*                                                                               
         MVI   BP4,1               INSERT IF NOT FOUND                          
         SAM31                                                                  
         GOTO1 =V(BINSRCH),BSPARS,WORK                                          
         SAM24                                                                  
         TM    0(R1),X'80'         TEST "RECORD NOT FOUND" BIT                  
         JZ    ERDUPDAT            DUPLICATE HOLIDAY DATE                       
*                                                                               
         OC    1(3,R1),1(R1)       TEST TABLE FULL                              
         JNZ   RHOL10              NO - READ NEXT HOLIDAY CARD                  
*                                                                               
         J     ERTABFUL            TABLE FULL                                   
*                                                                               
HOLEOF   DS    0H                                                               
         CLOSE HOLIDAYS                                                         
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
ERRMSGLQ EQU   30                                                               
*                               ----+----1----+----2----+----3                  
ERNOOPEN LA    R1,=CL(ERRMSGLQ)'CANNOT OPEN HOLIDAYS DATASET'                   
         J     ERRDIE                                                           
ERTABEMP LA    R1,=CL(ERRMSGLQ)'BINSRCH HOLIDAY TABLE EMPTY'                    
         J     ERRDIE                                                           
ERTABLOK LA    R1,=CL(ERRMSGLQ)'CAN''T LOCK DATASPACE TABLE'                    
         J     ERRDIE                                                           
ERNOOBT  LA    R1,=CL(ERRMSGLQ)'BINSRCH STORAGE NOT OBTAINED'                   
         J     ERRDIE                                                           
ERBADCTR LA    R1,=CL(ERRMSGLQ)'BAD COUNTRY CODE'                               
         J     ERRDIE                                                           
ERBADDAT LA    R1,=CL(ERRMSGLQ)'INVALID DATE ON HOLIDAY CARD'                   
         J     ERRDIE                                                           
ERGETDAY LA    R1,=CL(ERRMSGLQ)'D.O.W. CAN''T BE DETERMINED'                    
         J     ERRDIE                                                           
ERDUPDAT LA    R1,=CL(ERRMSGLQ)'DUPLICATE HOLIDAY DATE'                         
         J     ERRDIE                                                           
ERTABFUL LA    R1,=CL(ERRMSGLQ)'BINSRCH HOLIDAY TABLE FULL'                     
         J     ERRDIE                                                           
ERMAXENT LA    R1,=CL(ERRMSGLQ)'MAX DATES PER COUNTRY EXCEEDED'                 
         J     ERRDIE                                                           
*                                                                               
ERRDIE   MVC   P(7),=CL8'ERROR: '                                               
         MVC   P+7(ERRMSGLQ),0(R1)                                              
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
$$DATA   LOCTR ,                   BACKUP TO RB LOCATION                        
         DS    0D                                                               
HOLIDAYS DCB   DDNAME=HOLIDAYS,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=HOLEOF          
*                                                                               
ARZERO   DC    16F'0'                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
$$CODE   LOCTR ,                   BACK TO CODE LOCATION                        
       ++INCLUDE FACTRYTAB         COUNTRY EQU, NAME TABLE                      
*                                                                               
*                                                                               
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
UTL      DS    0D                                                               
         DC    F'0'                                                             
         DC    X'0A'               CONTROL SYSTEM                               
*                                                                               
COMFACS  DS    0D                                                               
       ++INCLUDE DDCOMFACSC                                                     
*                                                                               
       ++INCLUDE DDMASTC                                                        
*                                                                               
TABSGTRX DS    0D                  END OF CSECT                                 
*                                                                               
*                                                                               
*                                                                               
* SEE DDHOLTABD FOR EQU DEFINITIONS                                             
HOLTABLQ EQU   HOLTCMXQ*(HOLTTTLQ+HOLTYMXQ*HOLTABNQ*HOLTABDLQ)                  
CRDTABLQ EQU   HOLTCMXQ*HOLTYMXQ*HOLTABNQ*(HOLTABDLQ+1)                         
*                                                                               
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
WORK     DS    XL64                                                             
WORK2    DS    XL64                                                             
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
FLAG     DS    X                                                                
*                                                                               
AHOLTAB  DS    A                                                                
*                                                                               
CARD     DS    CL80                                                             
WRITEFL  DS    C                                                                
BACKFL   DS    C                                                                
LOOPFL   DS    C                                                                
HOLSTFL  DS    C                                                                
HOLENDFL DS    C                                                                
*                                                                               
         DS    0F                                                               
BSPARS   DS    0XL32               BINSEARCH PARAMETERS                         
BP1      DS    F                                                                
BP2      DS    F                                                                
BP3      DS    F                                                                
BP4      DS    F                                                                
BP5      DS    F                                                                
BP6      DS    F                                                                
BP7      DS    F                                                                
BP8      DS    F                                                                
*                                                                               
WORKDLQ EQU    *-WORKD                                                          
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENPHASE                                                     
       ++INCLUDE DDGETRETD                                                      
       ++INCLUDE FACTRYEQUS                                                     
       ++INCLUDE DDHOLTABD                                                      
       ++INCLUDE FACTRY            CTRYTABD                                     
       ++INCLUDE DMSPACED                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATABSDEQU                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002FATABSGTR 10/30/18'                                      
         END                                                                    
