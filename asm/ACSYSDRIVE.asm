*          DATA SET ACSYSDRIVE AT LEVEL 009 AS OF 05/01/03                      
*PHASE T00A81A                                                                  
*INCLUDE ACSLRY                                                                 
         TITLE 'ACDRIVER - SYSTEM DRIVER FOR ACCPAK'                            
ACDRIVER CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACDV**,R8,R7,R6,RR=R2                                        
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   GLHOOK,GLRESOLV                                                  
         BNE   AC2                                                              
         BAS   RE,SYSRES           EITHER RESOLVING ADDRESSES                   
         B     XIT                                                              
         SPACE 1                                                                
AC2      CLI   GLHOOK,GLROUT                                                    
         BNE   AC4                                                              
         BAS   RE,SYSEXEC          OR EXECUTING ROUTINES                        
         SPACE 1                                                                
AC4      CLI   GLHOOK,GLPUTSRT                                                  
         B     XIT                                                              
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              RESOLVING ROUTINE ADDRESSES                                      
         SPACE 3                                                                
SYSRES   NTR1                                                                   
         L     R1,=A(ROUTLIST)                                                  
         SPACE 1                                                                
SYSRES2  CLC   0(8,R1),GLLABEL                                                  
         BE    SYSRES4                                                          
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         B     SYSRES2                                                          
         SPACE 1                                                                
SYSRES4  MVC   GLAROUT,8(R1)       RETURN ADDRESS                               
         B     XIT                                                              
         EJECT                                                                  
*              EXECUTING ROUTINES (ROWS)                                        
         SPACE 3                                                                
SYSEXEC  NTR1                                                                   
         MVC   WORK,MYSPACES       PRESET WORK AREAS                            
         ZAP   DUB,=P'0'                                                        
         L     RF,GLAROUT                                                       
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         SPACE 1                                                                
         CLI   GLMODE,GLOUTPUT     PRESET SOME FIELDS FOR OUTPUT                
         BNER  RF                                                               
         MVC   OUTAREA,MYSPACES                                                 
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         CLI   DROLTYP,C'N'        NO PRINT - NOT INTERESTED                    
         BE    XIT                                                              
         MVC   MYPOSO,DROPOS                                                    
         MVC   MYOLEN,DROLEN                                                    
         DROP  R1                                                               
         BR    RF                                                               
         EJECT                                                                  
*              ACCOUNT INPUT                                                    
         SPACE 3                                                                
*              ARGUMENTS           1 0=LOW    1-4=LEVELS 1-4                    
*                                    5=CONTRA 6-9=CONTRA 1-4                    
*                                    11-13    PROJECT CONTROL 1-3               
         SPACE 1                                                                
ACCIN    ZIC   R5,GLARGS           PICK UP LEVEL                                
         MVC   0(15,R2),ACIOACC                                                 
         LTR   R5,R5                                                            
         BZ    NAMEIN              0=ACCOUNT                                    
         CH    R5,=H'5'                                                         
         BL    ACCIN2              1-4=ACCOUNT LEVEL 1-4                        
         MVC   0(15,R2),ACIOCON                                                 
         BE    NAMEIN                                                           
         SH    R5,=H'5'                                                         
         CH    R5,=H'5'                                                         
         BL    ACCIN2              11-13 PROJECT CONTROL                        
         SH    R5,=H'5'                                                         
         XC    0(15,R2),0(R2)                                                   
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'51'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACPCD,R4                                                         
         CLI   ACPCPRJT+3,C'A'                                                  
         BL    XIT                                                              
         MVC   0(1,R2),ACIOACC                                                  
         MVC   1(2,R2),=C'1J'                                                   
         MVC   3(12,R2),ACPCPRJT+3                                              
         CLI   GLARGS+2,C'R'                                                    
         BNE   ACCIN2                                                           
         MVC   0(3,R2),ACPCCLI                                                  
         SPACE 1                                                                
ACCIN2   GOTO1 GETCODE,DMCB,((R5),(R2)),BLOCK,BLOCK+15                          
         CLI   BLOCK+15,C'A'                                                    
         BNL   ACCIN4                                                           
         XC    0(15,R2),0(R2)      NO APPLICABLE CODE                           
         B     XIT                                                              
         SPACE 1                                                                
ACCIN4   MVC   0(15,R2),BLOCK                                                   
         CLI   GLARGS+1,C'C'       IF CODE, JUST NEED PIECE                     
         BNE   NAMEIN                                                           
         MVC   3(12,R2),BLOCK+15                                                
         B     XIT                                                              
         SPACE 1                                                                
NAMEIN   CLI   GLARGS+1,C'N'       IF NAME SEQUENCE WAS NEEDED                  
         BNE   XIT                 GET NAME OUT NOW FOR KEY                     
         GOTO1 GETNAME,DMCB,0(R2),BLOCK                                         
         MVC   36(3,R2),0(R2)      PASS CUL AT END                              
         MVC   0(36,R2),BLOCK                                                   
         B     XIT                                                              
         EJECT                                                                  
*              TALENT ACCOUNT INPUT                                             
         SPACE 3                                                                
*              ARGUMENTS           1 1=CLIENT 2=PRODUCT 3=COMM                  
*                                    4=EMPLOYER 5=PERFORMER                     
         SPACE 1                                                                
TALIN    CLC   ACIOACC+1(2),=C'SJ' USE ACCIN FOR PRODUCTION                     
         BE    ACCIN                                                            
         CLC   ACIOACC+1(2),=C'SV' CHECK FOR PAYABLES                           
         BE    CLIPAY                                                           
         CLC   ACIOACC+1(2),=C'SW'                                              
         BE    CLIPAY                                                           
         CLC   ACIOACC+1(2),=C'SX'                                              
         BE    CLIPAY                                                           
         CLC   ACIOACC+1(2),=C'SM' USE THIS FOR TALENT                          
         BE    TALIN1                                                           
         CLC   ACIOACC+1(2),=C'SN'                                              
         BE    TALIN1                                                           
         CLI   ACIOACC+1,C'T'                                                   
         BNE   CLIIN               OTHERWISE ELSEWHERE                          
         SPACE 1                                                                
TALIN1   MVC   0(15,R2),MYSPACES   PRECLEAR ACCOUNT                             
*&&US                                                                           
         CLI   ACIOTLT,C'C'        FOR COMMERCIALS                              
         BNE   TALIN2                                                           
         CLI   GLARGS,4            CLI/PROD/COMM USE ACCIN                      
         BL    ACCIN                                                            
         CLC   ACIOCON(3),=C'999'  CAN'T GET EMP/PERF FROM BILLING              
         BE    XIT                                                              
         CLC   ACIOCON(2),=C'*A'         OR FROM HISTORY                        
         BE    XIT                                                              
         MVC   0(1,R2),ACIOACC     ELSE USE COMPANY                             
         MVC   1(2,R2),=C'SN'      THEN SN                                      
         MVC   3(3,R2),ACIOCON     THEN EMPLOYER                                
         CLI   GLARGS,4                                                         
         BE    NAMEIN              (4=EMPLOYER NOW DONE)                        
         MVC   6(9,R2),ACIOCON+3   THEN PERFORMER                               
         B     NAMEIN                                                           
         SPACE 1                                                                
*                                  FOR PERFORMERS LEDGER                        
TALIN2   CLI   GLARGS,3            EMPLOYER/PERFORMER USE ACCIN                 
         BH    TALIN4                                                           
         CLC   ACIOCON(3),=C'999'  CAN'T GET CLI/PROD/COMM                      
         BE    TALIN6                    FROM PAYROLL HERE                      
         CLC   ACIOCON(2),=C'*A'                                                
         BE    XIT                                                              
         MVC   0(1,R2),ACIOACC     ELSE USE COMPANY                             
         MVC   1(2,R2),ACIOWORK    THEN U/L FROM WORK CODE                      
         MVC   3(3,R2),ACIOCON     THEN CLIENT                                  
         CLI   GLARGS,2                                                         
         BL    NAMEIN              (1=CLIENT NOW DONE)                          
         MVC   6(3,R2),ACIOCON+3   THEN PRODUCT                                 
         BE    NAMEIN              (2=PRODUCT NOW DONE)                         
         MVC   9(6,R2),ACIOCON+6   THEN COMMERCIAL                              
         B     NAMEIN                                                           
         SPACE 1                                                                
TALIN4   ZIC   R1,GLARGS           EMPLOYER/PERFORMER (4/5)                     
         SH    R1,=H'3'                                                         
         STC   R1,GLARGS           BECOME 1/2                                   
         B     ACCIN               AND USE ACCIN                                
         SPACE 1                                                                
CATIN    CLC   ACIOWORK,=C'99'                                                  
         BE    TALIN6                                                           
         MVC   0(3,R2),ACIOCON+12  CATEGORY END OF CONTRA A/C                   
         B     XIT                                                              
         SPACE 1                                                                
TALIN6   CLI   ACMODE,PROCTRNS     ANALYSIS FROM X'4F' ELEMENT                  
         BE    TALIN8                                                           
         CLI   ACMODE,PROC83                                                    
         BNE   XIT                                                              
         SPACE 1                                                                
TALIN8   L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'4F'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TRCPJD,R4                                                        
         CLI   GLARGS,3                                                         
         BH    XIT                                                              
         MVC   0(3,R2),TRCPCAT     CATEGORY                                     
         CLI   GLARGS,0                                                         
         BE    XIT                                                              
         MVI   0(R2),0                                                          
         MVC   1(3,R2),TRCPCLI     CLIENT                                       
         CLI   GLARGS,1                                                         
         BE    XIT                                                              
         MVC   1(3,R2),TRCPPROD    PRODUCT                                      
         CLI   GLARGS,2                                                         
         BE    XIT                                                              
         MVC   1(6,R2),TRCPCOM     COMMERCIAL                                   
         B     XIT                                                              
         DROP  R4                                                               
*&&                                                                             
         EJECT                                                                  
*              TALENT DPS AGENCY HANDLING                                       
         SPACE 3                                                                
*&&US                                                                           
AGYIN    MVC   0(3,R2),ACIOACC     PICK UP CUL FOR COMMERCIALS                  
         CLI   ACIOTLT,C'C'                                                     
         BE    XIT                                                              
         MVC   1(2,R2),ACIOWORK    ELSE PICK UP U/L FROM WORK CODE              
         CLC   ACIOWORK,=C'99'     UNLESS THIS IS PAYROLL                       
         BNE   XIT                                                              
         XC    1(2,R2),1(R2)                                                    
         CLI   ACMODE,PROCTRNS                                                  
         BE    AGYIN2                                                           
         CLI   ACMODE,PROC83                                                    
         BNE   XIT                                                              
         SPACE 1                                                                
AGYIN2   L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'4F'        LOOK IN X'4F' ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TRCPJD,R4                                                        
         MVC   1(2,R2),TRCPUL      FOR UNIT/LEDGER                              
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 1                                                                
AGYOUT   MVC   LABLAREA(6),=C'AGENCY'                                           
         CLI   GLARGS+1,C'N'                                                    
         BE    AGYOUT2                                                          
         MVC   CODEAREA(1),1(R2)   UNIT                                         
         GOTO1 HEXOUT,DMCB,2(R2),CODEAREA+1,1,=C'TOG'                           
         SPACE 1                                                                
AGYOUT2  CLI   GLARGS+1,C'C'                                                    
         BE    GENOUT                                                           
         MVC   WORK(3),0(R2)                                                    
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         B     XIT                                                              
         SPACE 1                                                                
PERFOUT  MVC   LABLAREA(9),=C'PERFORMER'                                        
         CLI   GLARGS+1,C'N'       NAME OPTION                                  
         BE    ACCOUT4                                                          
         MVC   CODEAREA(3),6(R2)   EDIT AS NNN-NN-NNNN                          
         MVI   CODEAREA+3,C'-'                                                  
         MVC   CODEAREA+4(2),9(R2)                                              
         MVI   CODEAREA+6,C'-'                                                  
         MVC   CODEAREA+7(4),11(R2)                                             
         B     ACCOUT1                                                          
*&&                                                                             
         EJECT                                                                  
*              CLIENT, PRODUCT JOB FOR PAYABLES                                 
         SPACE 3                                                                
*              ARGUMENTS           1 1=CLIENT 2=PRODUCT 3=JOB                   
*                                  2 C=CODE N=NAME B=BOTH                       
         SPACE 1                                                                
CLIPAY   CLI   ACMODE,PROCTRNS     INPUT ROUTINES                               
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         USING ACKEYD,R4                                                        
         MVC   0(15,R2),ACIOCON    CLIENT IS IN CONTRA ACCOUNT                  
         ZIC   R5,GLARGS                                                        
         CLI   GLARGS,1                                                         
         BE    ACCIN2                                                           
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   CLIPAY2                                                          
         USING ACOTHERD,R4                                                      
         MVC   SAVOTNUM,ACOTNUM    SAVE PRODUCT/JOB FROM CREDIT                 
         SPACE 1                                                                
CLIPAY2  MVC   6(3,R2),SAVOTNUM    PRODUCT IN OTHERD                            
         CLI   GLARGS,2                                                         
         BE    ACCIN2                                                           
         MVC   9(6,R2),SAVOTNUM+6  SO IS JOB NUMBER                             
         B     ACCIN2                                                           
         SPACE 1                                                                
SAVOTNUM DC    CL12' '                                                          
         EJECT                                                                  
*              CLIENT AND PRODUCT                                               
         SPACE 3                                                                
*              ARGUMENTS           1 1=CLIENT 2=PRODUCT 3=COMMERCIAL            
*                                  2 C=CODE N=NAME B=BOTH                       
*              INPUT/OUTPUT        BYTE 1     0=PAYABLE/RECEIVABLE              
*                                             NON ZERO = OTHER                  
*                                  BYTES 2-4  CLIENT/PRODUCT CODE               
*                                  BYTES 5-24 CLIENT/PRODUCT NAME               
         SPACE 1                                                                
CLIIN    MVI   0(R2),0             INPUT ROUTINES                               
         L     R4,ADTRNREC                                                      
         USING ACKEYD,R4                                                        
         CLI   GLARGS+1,C'N'                                                    
         BE    CLIIN2                                                           
         MVC   1(3,R2),ACIOCON+12  CLIENT END OF CONTRA A/C                     
         CLI   GLARGS,1                                                         
         BE    CLIIN2                                                           
         MVC   1(3,R2),ACKEYREF    PRODUCT FIRST 3 OF REF                       
         SPACE 1                                                                
CLIIN2   CLI   GLARGS+1,C'C'                                                    
         BE    XIT                                                              
         MVI   ELCODE,X'46'        NAMES ARE IN X'46' ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    CLIIN4                                                           
         LA    R4,SAVE46                                                        
         B     CLIIN6                                                           
         SPACE 1                                                                
CLIIN4   MVC   SAVE46,0(R4)                                                     
         USING TRPAYD,R4                                                        
CLIIN6   MVC   4(20,R2),TRPYCLI                                                 
         CLI   GLARGS,2                                                         
         BNE   *+10                                                             
         MVC   4(20,R2),TRPYPROD                                                
         B     XIT                                                              
         SPACE 3                                                                
CLIOUT   CLI   0(R2),0             OUTPUT ROUTINES                              
         BNE   ACCOUT                                                           
         MVC   LABLAREA(6),=C'CLIENT'                                           
         CLI   GLARGS,2                                                         
         BNE   *+10                                                             
         MVC   LABLAREA(7),=C'PRODUCT'                                          
         MVC   CODEAREA(3),1(R2)                                                
         CLI   GLARGS+1,C'C'                                                    
         BE    *+10                                                             
         MVC   NAMEAREA(20),4(R2)                                               
         CLI   GLARGS,3                                                         
         BNE   GENOUT                                                           
         MVC   LABLAREA(10),=C'COMMERCIAL'                                      
         MVC   CODEAREA(6),1(R2)                                                
         MVC   NAMEAREA,MYSPACES                                                
         B     GENOUT                                                           
         EJECT                                                                  
*              ACCOUNT OUTPUT                                                   
         SPACE 3                                                                
*              ARGUMENTS           1 0=LOW    1-4=LEVELS 1-4                    
*                                    5=CONTRA 6-9=CONTRA 1-4                    
*                                    11-13 PROJECT CONTROL                      
*                                  2 C=CODE N=NAME B=BOTH                       
ACCOUT   ZIC   R5,GLARGS                                                        
         CH    R5,=H'5'                                                         
         BL    *+8                                                              
         SH    R5,=H'5'                                                         
         CH    R5,=H'5'                                                         
         BL    *+8                                                              
         SH    R5,=H'5'                                                         
         CLI   GLARGS+1,C'N'                                                    
         BE    ACCOUT4                                                          
         CLI   GLARGS,5            IF WE ARE SHOWING CONTRA-ACCOUNT             
         BNE   ACCOUTB                                                          
         MVC   CODEAREA(14),1(R2)  MOVE OUT U/L/ACCOUNT                         
         B     ACCOUT1                                                          
         SPACE 1                                                                
ACCOUTB  MVC   CODEAREA(12),3(R2)                                               
         CLI   GLARGS+1,C'C'                                                    
         BE    ACCOUT6                                                          
         MVC   CODEAREA(12),MYSPACES                                            
         GOTO1 GETCODE,DMCB,((R5),0(R2)),BLOCK,CODEAREA                         
*                                  NOW HAVE PIECE OF ACCOUNT                    
         SPACE 1                                                                
ACCOUT1  CLI   GLARGS+1,C'C'                                                    
         BE    ACCOUT6                                                          
         SPACE 1                                                                
ACCOUT2  GOTO1 GETNAME,DMCB,(R2),NAMEAREA                                       
         B     ACCOUT6                                                          
         SPACE 1                                                                
ACCOUT4  MVC   NAMEAREA(36),0(R2)                                               
         LA    R2,36(R2)           CARRYING C/U/L AFTER NAMES                   
         SPACE 1                                                                
ACCOUT6  TM    GLINDS,X'40'                                                     
         BO    ACCOUT8                                                          
         CLI   MYLTYP,C'H'                                                      
         BNE   GENOUT                                                           
         SPACE 1                                                                
ACCOUT8  GOTO1 GETLABEL,DMCB,((R5),0(R2)),LABLAREA                              
         B     GENOUT                                                           
         EJECT                                                                  
*              SPECIAL TOTAL ROUTINE FOR ACCOUNTS                               
         SPACE 3                                                                
*              ARGUMENTS           1 0=LOW    1-4=LEVELS 1-4                    
*                                    5=CONTRA 6-9=CONTRA 1-4                    
*                                  2 C=CODE N=NAME B=BOTH                       
*              INPUT               ROW1WIDE ROWWIDTH                            
         SPACE 1                                                                
ACCTOTAL B     ACCOUT                                                           
         MVC   BLOCK(100),MYSPACES                                              
         MVC   0(8,R3),=C'*TOTALS*'                                             
         CLI   ROWWIDTH,20                                                      
         BL    XIT                                                              
         MVC   BLOCK(11),=C'*TOTALS FOR'                                        
         ZIC   R5,GLARGS                                                        
         CH    R5,=H'5'                                                         
         BL    *+8                                                              
         SH    R5,=H'5'                                                         
         CH    R5,=H'5'                                                         
         BL    *+8                                                              
         SH    R5,=H'5'                                                         
         CLI   GLARGS+1,C'N'       IF THIS IS A NAME                            
         BNE   ACCTOT2                                                          
         MVC   BLOCK+50(36),0(R2)  MOVE IT OUT NOW                              
         MVC   0(3,R2),36(R2)      AND SHUFFLE UP THE C/U/L                     
         SPACE 1                                                                
ACCTOT2  GOTO1 GETLABEL,DMCB,((R5),0(R2)),BLOCK+12                              
         CLI   GLARGS+1,C'N'                                                    
         BE    ACCTOT4                                                          
         CLI   GLARGS,5            IF WE ARE SHOWING CONTRA-ACCOUNT             
         BNE   ACCTOT2B                                                         
         MVC   BLOCK+30(14),1(R2)  MOVE OUT U/L/ACCOUNT                         
         B     ACCTOT3                                                          
         SPACE 1                                                                
ACCTOT2B GOTO1 GETCODE,DMCB,((R5),0(R2)),WORK,BLOCK+30                          
*                                  NOW HAVE PIECE OF ACCOUNT                    
ACCTOT3  CLI   GLARGS+1,C'C'                                                    
         BE    ACCTOT4                                                          
         GOTO1 GETNAME,DMCB,(R2),BLOCK+50                                       
         SPACE 1                                                                
ACCTOT4  GOTO1 SQUASHER,DMCB,BLOCK,100                                          
         ZIC   R4,ROW1WIDE                                                      
         CLI   ROW1WIDE,19                                                      
         BH    ACCTOT6                                                          
         ZIC   R4,ROWWIDTH                                                      
         SPACE 1                                                                
ACCTOT6  BCTR  R4,0                                                             
         LA    R1,4                                                             
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198                                                       
         GOTO1 CHOPPER,DMCB,(100,BLOCK),((R4),0(R3))                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR ATTRIBUTE PIECES                                    
         SPACE 3                                                                
*              ARGUMENTS           1 0=LOW    1-4=LEVELS 1-4                    
*                                  2 ATTRIBUTE UNIT                             
*                                  3 ATTRIBUTE LEDGER                           
         SPACE 1                                                                
ATTIN    CLI   ACMODE,PROCTRNS     ONLY GOOD AT TRANSACTION LEVEL               
         BNE   XIT                                                              
         L     R1,ADATTR           PICK UP ADDRESS OF ATTRIB ELEMENT            
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         USING ACAPD,R1                                                         
         LA    R3,ACAPMLEN                                                      
         ZIC   R0,ACAPMIN                                                       
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         DROP  R1                                                               
         USING ACAPMLEN,R3                                                      
         SPACE 1                                                                
ATTIN2   CLC   ACAPACCT(2),GLARGS+1   MUST MATCH ON U/L                         
         BE    ATTIN4                                                           
         ZIC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         BCT   R0,ATTIN2                                                        
         B     XIT                                                              
         SPACE 1                                                                
ATTIN4   MVC   0(15,R2),MYSPACES                                                
         MVC   0(1,R2),ACIOACC     COMPANY                                      
         ZIC   R1,0(R3)            (L'MINI ELEMENT)                             
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),ACAPACCT                                                 
         ZIC   R5,GLARGS                                                        
         LTR   R5,R5                                                            
         BZ    ATTIN6              0=ACCOUNT                                    
         GOTO1 GETCODE,DMCB,((R5),(R2)),BLOCK,BLOCK+15                          
         MVC   0(15,R2),BLOCK                                                   
         SPACE 1                                                                
ATTIN6   CLI   GLARGS+3,C'N'       IF NAME SEQUENCE WAS NEEDED                  
         BNE   XIT                 GET NAME OUT NOW FOR KEY                     
         GOTO1 GETNAME,DMCB,0(R2),BLOCK                                         
         MVC   36(3,R2),0(R2)      PASS CUL AT END                              
         MVC   0(36,R2),BLOCK                                                   
         B     XIT                                                              
         SPACE 1                                                                
ATTOUT   ZIC   R5,GLARGS                                                        
         CLI   GLARGS+1,C'N'                                                    
         BE    ATTOUT4                                                          
         GOTO1 GETCODE,DMCB,((R5),0(R2)),BLOCK,CODEAREA                         
*                                  NOW HAVE PIECE OF ACCOUNT                    
         CLI   GLARGS+1,C'C'                                                    
         BE    ATTOUT6                                                          
         GOTO1 GETNAME,DMCB,(R2),NAMEAREA                                       
         B     ATTOUT6                                                          
         SPACE 1                                                                
ATTOUT4  MVC   NAMEAREA(36),0(R2)                                               
         LA    R2,36(R2)           CARRYING C/U/L AFTER NAMES                   
         SPACE 1                                                                
ATTOUT6  TM    GLINDS,X'40'                                                     
         BO    ATTOUT8                                                          
         CLI   MYLTYP,C'H'                                                      
         BNE   GENOUT                                                           
         SPACE 1                                                                
ATTOUT8  GOTO1 GETLABEL,DMCB,((R5),0(R2)),LABLAREA                              
         B     GENOUT                                                           
         DROP  R3                                                               
         EJECT                                                                  
*              SPECIAL ATTRIBUTE ROUTINES - VENDOR                              
         SPACE 3                                                                
*              ATTRIBUTE 1         C=CODE N=NAME B=BOTH                         
         SPACE 1                                                                
VENDIN   CLI   ACMODE,PROCTRNS     ONLY GOOD AT TRANSACTION LEVEL               
         BNE   XIT                                                              
         L     R1,ADATTR           PICK UP ADDRESS OF ATTRIB ELEMENT            
         LTR   R1,R1                                                            
         BZ    VENDIN4                                                          
         USING ACAPD,R1                                                         
         LA    R3,ACAPMLEN                                                      
         ZIC   R0,ACAPMIN                                                       
         DROP  R1                                                               
         USING ACAPMLEN,R3                                                      
         SPACE 1                                                                
VENDIN2  CLC   ACAPACCT(2),=C'SX'  MUST MATCH ON SX,                            
         BE    VENDIN6                                                          
         CLC   ACAPACCT(2),=C'SV'                SV,                            
         BE    VENDIN6                                                          
         CLC   ACAPACCT(2),=C'2C'             OR 2C                             
         BE    VENDIN6                                                          
         ZIC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         BCT   R0,VENDIN2                                                       
         SPACE 1                                                                
VENDIN4  MVC   0(15,R2),ACIOCON    ELSE PICK UP CONTRA ACCOUNT                  
         B     VENDIN8                                                          
         SPACE 1                                                                
VENDIN6  MVC   0(15,R2),MYSPACES                                                
         MVC   0(1,R2),ACIOACC     COMPANY                                      
         ZIC   R1,0(R3)            (L'MINI ELEMENT)                             
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),ACAPACCT                                                 
         SPACE 1                                                                
VENDIN8  CLI   GLARGS,C'N'         IF NAME SEQUENCE WAS NEEDED                  
         BNE   XIT                 GET NAME OUT NOW FOR KEY                     
         GOTO1 GETNAME,DMCB,0(R2),BLOCK                                         
         MVC   0(36,R2),BLOCK                                                   
         OC    0(36,R2),MYSPACES                                                
         B     XIT                                                              
         SPACE 1                                                                
VENDOUT  MVC   LABLAREA(6),=C'VENDOR'                                           
         CLI   GLARGS,C'N'                                                      
         BE    VENDOUT2                                                         
         MVC   CODEAREA(12),3(R2)                                               
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         GOTO1 GETNAME,DMCB,(R2),NAMEAREA                                       
         B     GENOUT                                                           
         SPACE 1                                                                
VENDOUT2 MVC   NAMEAREA(36),0(R2)                                               
         B     GENOUT                                                           
         EJECT                                                                  
*              SHARED OUTPUT ROUTINE                                            
         SPACE 3                                                                
*              AT THIS STAGE...    LABLAREA HAS PREFIX                          
*                                  CODEAREA HAS CODE                            
*                                  NAMEAREA HAS NAME                            
         SPACE 1                                                                
GENOUT   TM    GLINDS,X'40'        TOTALS ARE DEALT WITH BELOW                  
         BO    TOTOUT                                                           
         CLI   MYLTYP,C'H'         FOR HEADLINES, MOVE OUT THE LOT              
         BNE   GENOUT2                                                          
         MVC   0(L'OUTAREA,R3),OUTAREA                                          
         B     XIT                                                              
         SPACE 1                                                                
GENOUT2  CLI   MYLTYP,C'M'         FOR MIDLINES, SQUASH FIRST                   
         BNE   GENOUT4                                                          
         GOTO1 SQUASHER,DMCB,CODENNAM,52                                        
         MVC   0(L'CODENNAM,R3),CODENNAM                                        
         B     XIT                                                              
         SPACE 1                                                                
GENOUT4  SR    R4,R4               ANY CODE TO OUTPUT                           
         CLC   CODEAREA,MYSPACES                                                
         BE    GENOUT10                                                         
         LA    R4,15                                                            
         LA    R1,CODEAREA+14                                                   
         SPACE 1                                                                
GENOUT6  CLI   0(R1),C' '          FIGURE OUT L'CODE                            
         BH    GENOUT8                                                          
         BCTR  R1,0                                                             
         BCT   R4,GENOUT6                                                       
         SPACE 1                                                                
GENOUT8  BCTR  R4,0                (R4=L'CODE-1)                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),CODEAREA    MOVE OUT THE CODE                            
         LA    R4,2(R4)            (NOW HAS LENGTH+1)                           
         AR    R3,R4                                                            
         SPACE 1                                                                
GENOUT10 LA    R1,NAMEAREA         SET UP TO CHOP THE NAME                      
         ST    R1,DMCB             A(INPUT)                                     
         MVI   DMCB,36             L'INPUT                                      
         ST    R3,DMCB+4           A(OUTPUT)                                    
         ZIC   R1,MYOLEN                                                        
         SR    R1,R4                                                            
         BNP   XIT                                                              
         STC   R1,DMCB+4                                                        
         LA    R1,4                MAX N'LINES                                  
         TM    GLDOWNLD,X'80'                                                   
         BNO   *+8                                                              
         LA    R1,1                ONLY 1 FOR DOWNLOADING                       
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198          PRINT LINES ARE 198 APART                    
         GOTO1 CHOPPER,DMCB                                                     
         B     XIT                                                              
         EJECT                                                                  
*              SHARED TOTAL ROUTINE                                             
         SPACE 3                                                                
*              AT THIS STAGE...    LABLAREA HAS PREFIX                          
*                                  CODEAREA HAS CODE                            
*                                  NAMEAREA HAS NAME                            
*              INPUT               ROW1WIDE ROWWIDTH                            
         SPACE 1                                                                
TOTOUT   DS    0H                                                               
         ZIC   R1,GLRECNO          PICK UP PRESENT RECORD NUMBER                
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,GLAINTD(R1)      GET TO DETAILS FOR THIS REPORT               
         L     R1,0(R1)                                                         
         USING GLINTD,R1                                                        
         LH    R3,GLPDISP          NOW PICK UP PRINT DISPLACEMENT               
         A     R3,GLAINTP1         AND GET ACTUAL PRINT ADDRESS                 
         LA    R3,1(R3)                                                         
         DROP  R1                                                               
         SPACE 1                                                                
*******  L     R1,GLATOUT          PICK UP A(OUT ELEMENT)                       
*******  USING DROD,R1                                                          
*******  ZIC   R4,DROLEN           AND USE THE OUT WIDTH                        
*******  CLI   DROLTYP,C'P'        IF FIELD IS IN THE PRINT LINE                
*******  BNE   TOTOUT1                                                          
*******  CH    R4,=H'13'                                                        
*******  BH    TOTOUT2                                                          
*******  DROP  R1                                                               
*******  SPACE 1                                                                
TOTOUT1  DS    0H                                                               
*******  ZIC   R4,ROW1WIDE                                                      
*******  BCTR  R4,0                                                             
*******  CLI   ROW1WIDE,13                                                      
*******  BH    TOTOUT2                                                          
         ZIC   R4,ROWWIDTH                                                      
         BCTR  R4,0                                                             
         SPACE 1                                                                
TOTOUT2  CH    R4,=H'3'                                                         
         BL    TOTOUTX                                                          
         MVC   0(3,R3),=C'ALL'                                                  
         CH    R4,=H'5'                                                         
         BL    TOTOUTX                                                          
         MVC   0(5,R3),=C'*ALL*'                                                
         CH    R4,=H'8'                                                         
         BL    TOTOUTX                                                          
         MVC   0(8,R3),=C'*TOTALS*'                                             
         MVC   BLOCK(80),MYSPACES                                               
         MVC   BLOCK(11),=C'*TOTALS FOR'                                        
         MVC   BLOCK+12(65),OUTAREA                                             
         GOTO1 SQUASHER,DMCB,BLOCK,80                                           
         LA    R1,4                                                             
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198                                                       
         GOTO1 CHOPPER,DMCB,(80,BLOCK),((R4),0(R3))                             
         SPACE 1                                                                
TOTOUTX  MVC   OUTAREA,MYSPACES                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ACCOUNT HEADINGS                                                 
         SPACE 3                                                                
*              ARGUMENTS           1 0=LOW    1-4=LEVELS 1-4                    
*                                    5=CONTRA 6-9=CONTRA 1-4                    
*                                  2 C=CODE N=NAME B=BOTH                       
ACCHEAD  CLI   GLARGS,4                                                         
         BH    XIT                                                              
         CLI   GLARGS+1,C'N'       IF WE'RE DEALING WITH NAMES                  
         BNE   *+8                                                              
         LA    R2,36(R2)           THE C/U/L IS AT THE END                      
         GOTO1 GETLABEL,DMCB,(GLARGS,(R2)),(R3)                                 
         B     XIT                                                              
         EJECT                                                                  
*              COMPANY ROUTINES                                                 
         SPACE 3                                                                
COMPIN   MVC   0(1,R2),ACIOACC                                                  
         B     XIT                                                              
         SPACE 1                                                                
COMPOUT  MVC   LABLAREA(7),=C'COMPANY'                                          
         CLI   GLARGS,C'N'                                                      
         BE    COMPOUT2                                                         
         LA    R2,0(R2)            REMOVE HIGH ORDER BIT                        
         GOTO1 HEXOUT,DMCB,(R2),CODEAREA,1,=C'TOG'                              
         SPACE 1                                                                
COMPOUT2 CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA,COMPNAME                                                
         MVC   WORK(1),0(R2)                                                    
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         SPACE 3                                                                
*              UNIT ROUTINES                                                    
         SPACE 3                                                                
UNITIN   MVC   0(2,R2),ACIOACC                                                  
         CLI   GLARGS,0                                                         
         BE    XIT                                                              
         MVC   0(2,R2),ACIOCON                                                  
         B     XIT                                                              
         SPACE 1                                                                
UNITOUT  MVC   LABLAREA(4),=C'UNIT'                                             
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(1),1(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   WORK(2),0(R2)                                                    
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         EJECT                                                                  
*              LEDGER ROUTINES                                                  
         SPACE 3                                                                
LEDGIN   MVC   0(3,R2),ACIOACC                                                  
         CLI   GLARGS,0                                                         
         BE    XIT                                                              
         MVC   0(3,R2),ACIOCON                                                  
         B     XIT                                                              
         SPACE 1                                                                
LEDGOUT  MVC   LABLAREA(6),=C'LEDGER'                                           
         CLI   GLARGS+1,C'N'                                                    
         BE    LEDGOUT2                                                         
         MVC   CODEAREA(2),1(R2)                                                
         CLI   1(R2),C'T'                                                       
         BNE   LEDGOUT2                                                         
         GOTO1 HEXOUT,DMCB,2(R2),CODEAREA+1,1,=C'TOG'                           
         SPACE 1                                                                
LEDGOUT2 CLI   GLARGS+1,C'C'                                                    
         BE    GENOUT                                                           
         MVC   WORK(3),0(R2)                                                    
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         EJECT                                                                  
*              OFFICE ROUTINES                                                  
         SPACE 3                                                                
OFFIN    MVC   0(2,R2),ACIONOFF                                                 
         CLI   GLARGS,C'T'         OPTION TO FORCE OFFICE FROM TRANS            
         BNE   OFFIN2                                                           
         CLI   ACMODE,PROCTRNS     OFFICE IS IN TRANSACTION                     
         BNE   OFFIN2              SO REJECT OTHERS                             
         L     R1,ADTRANS                                                       
         USING TRANSD,R1                                                        
         MVC   0(2,R2),TRNSOFFC                                                 
         CLI   0(R2),C' '       IF MISSING                                      
         BH    OFFIN2                                                           
         MVC   0(2,R2),=C'??'        RETURN ??                                  
         SPACE 1                                                                
OFFIN2   L     R1,ADCMPEL                                                       
         USING ACCOMPD,R1                                                       
         TM    ACMPSTA4,X'01'      ?2 BYTE OFFICES                              
         BO    XIT                                                              
         MVI   1(R2),0                                                          
         B     XIT                                                              
         SPACE 1                                                                
OFFOUT   MVC   LABLAREA(6),=C'OFFICE'                                           
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(2),0(R2)                                                
         OI    CODEAREA+1,C' '    MAY BE 1 OR 2 BYTES                           
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(7),=C'MISSING'                                          
         CLI   0(R2),C'?'                                                       
         BE    GENOUT                                                           
         MVC   NAMEAREA(7),MYSPACES                                             
         MVC   WORK,MYSPACES                                                    
         MVC   WORK(1),AGENCY      MAY FIND NAME IN 2D LEDGER                   
         MVC   WORK+1(2),=C'2D'                                                 
         MVC   WORK+3(1),0(R2)                                                  
         CLI   1(R2),0                                                          
         BE    OFFOUT2                                                          
         MVI   WORK,X'01'          2 BYTE OFFICES                               
         MVC   WORK+1(1),AGENCY                                                 
         MVC   WORK+2(2),0(R2)                                                  
         SPACE 1                                                                
OFFOUT2  GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         EJECT                                                                  
*              OFFICE GROUP ROUTINES                                            
         SPACE 3                                                                
OGIN     MVC   0(1,R2),ACIOOG                                                   
         B     XIT                                                              
         SPACE 1                                                                
OGOUT    MVC   LABLAREA(12),=C'OFFICE GROUP'                                    
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(1),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         XC    WORK,WORK                                                        
         MVI   WORK,X'2C'                                                       
         MVI   WORK+1,X'02'                                                     
         MVC   WORK+2(3),ACIOACC                                                
         MVC   WORK+5(1),0(R2)                                                  
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         EJECT                                                                  
*              COSTING ROUTINES                                                 
         SPACE 3                                                                
COSTIN   MVC   0(1,R2),ACIOCOST                                                 
         B     XIT                                                              
         SPACE 1                                                                
COSTOUT  MVC   LABLAREA(7),=C'COSTING'                                          
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(1),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   WORK,MYSPACES                                                    
         MVC   WORK(1),WRICOMP                                                  
         MVC   WORK+1(2),=C'14'    GET NAMES FROM 14 LEDGER                     
         MVC   WORK+3(1),0(R2)                                                  
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         SPACE 3                                                                
         EJECT                                                                  
*              MEDIA ROUTINES                                                   
         SPACE 3                                                                
MEDIN    MVC   0(1,R2),ACIOMED                                                  
         B     XIT                                                              
         SPACE 1                                                                
MEDOUT   MVC   LABLAREA(5),=C'MEDIA'                                            
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(1),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   WORK,MYSPACES                                                    
         MVI   WORK,X'09'                                                       
         MVC   WORK+1(1),WRICOMP                                                
         MVC   WORK+2(1),0(R2)                                                  
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         SPACE 3                                                                
*              MEDIA GROUP ROUTINES                                             
         SPACE 3                                                                
MGIN     MVC   0(1,R2),ACIOMG                                                   
         B     XIT                                                              
         SPACE 1                                                                
MGOUT    MVC   LABLAREA(11),=C'MEDIA GROUP'                                     
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(1),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         XC    WORK,WORK                                                        
         MVI   WORK,X'2C'                                                       
         MVI   WORK+1,X'06'                                                     
         MVC   WORK+2(3),ACIOACC                                                
         MVC   WORK+5(1),0(R2)                                                  
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         EJECT                                                                  
*              WORK CODES                                                       
         SPACE 3                                                                
WORKIN   MVC   0(3,R2),ACIOACC     PASS C/U/L                                   
         MVC   3(2,R2),ACIOWORK    AND WORK CODE                                
         CLI   GLARGS,1                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
         XC    0(5,R2),0(R2)                                                    
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         L     R4,ADTRNREC         ARG 1=1  PROJECT CONTROL TASK                
         MVI   ELCODE,X'51'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACPCD,R4                                                         
         CLI   ACPCTSK,C'A'                                                     
         BL    XIT                                                              
         MVC   0(1,R2),ACIOACC     PASS COMPANY                                 
         MVC   1(2,R2),=C'1J'      U/L 1J                                       
         CLI   GLARGS+1,C'R'       (RTASK LOOKS FOR SJ WORKCODES)               
         BNE   *+8                                                              
         MVI   1(R2),C'S'                                                       
         MVC   3(2,R2),ACPCTSK                                                  
         B     XIT                                                              
         SPACE 1                                                                
WORKOUT  MVC   LABLAREA(8),=C'ANALYSIS'                                         
         CLC   0(2,R2),=C'SJ'                                                   
         BNE   *+10                                                             
         MVC   LABLAREA(9),=C'WORK CODE'                                        
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(2),3(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVC   WORK,MYSPACES                                                    
         MVI   WORK,X'0A'                                                       
         MVC   WORK+1(5),0(R2)                                                  
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         EJECT                                                                  
*              WORK GROUP ROUTINES                                              
         SPACE 3                                                                
WGIN     MVC   0(1,R2),ACIOWG                                                   
         B     XIT                                                              
         SPACE 1                                                                
WGOUT    MVC   LABLAREA(10),=C'WORK GROUP'                                      
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(1),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         XC    WORK,WORK                                                        
         MVI   WORK,X'2C'                                                       
         MVI   WORK+1,X'08'                                                     
         MVC   WORK+2(3),ACIOACC                                                
         MVC   WORK+5(1),0(R2)                                                  
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         EJECT                                                                  
*              T&E WORK CODES                                                   
         SPACE 3                                                                
*              ARGUMENTS           1=WORK CODE                                  
*                                  2=SUB CODE - 'OFFICE'                        
         SPACE 1                                                                
TEIN     CLI   FIRST86,1           IF 1ST TIME IN                               
         BE    TEIN2                                                            
         XC    PROF86,PROF86                                                    
         MVC   WORK(4),=C'A086'                                                 
         MVC   WORK+4(1),WRICOMP                                                
         MVC   WORK+12(2),AGYALPHA                                              
         GOTO1 GETPROF,DMCB,WORK,PROF86,DATAMGR                                 
         MVI   FIRST86,1                                                        
         SPACE 1                                                                
TEIN2    ZIC   R3,PROF86+1         POSITION IN CONTRA OF TE CODE                
         LTR   R3,R3                                                            
         BZ    *+8                                                              
         SH    R3,=H'2'                                                         
         LA    R4,ACIOCON+1(R3)                                                 
         MVC   0(1,R2),2(R4)       PUTTING OUT 'OFFICE' BYTE                    
         CLI   PROF86+3,C'Y'       IS WORK CODE 1ST                             
         BE    *+10                                                             
         MVC   0(1,R2),0(R4)                                                    
         CLI   GLARGS,2            (DONE IF ARG=2)                              
         BE    XIT                                                              
         CLI   PROF86+3,C'Y'       PUTTING OUT 'WORK CODE'                      
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         MVC   0(2,R2),0(R4)                                                    
         B     XIT                                                              
         SPACE 1                                                                
TEOUT    MVC   LABLAREA(8),=C'T&&E TYPE'                                        
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(2),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVI   WORK,X'0A'                                                       
         MVC   WORK+1(1),WRICOMP                                                
         MVC   WORK+2(2),ACIOACC+1                                              
         MVC   WORK+4(2),0(R2)                                                  
         GOTO1 GETNAME,DMCB,WORK,NAMEAREA                                       
         B     GENOUT                                                           
         EJECT                                                                  
*              FILTER CODE                                                      
         SPACE 3                                                                
FILTIN   ZIC   R1,GLARGS                                                        
         LA    R1,ACIOFILT-1(R1)                                                
         MVC   0(1,R2),0(R1)                                                    
         CLI   GLARGS,5                                                         
         BL    XIT                                                              
         MVC   0(1,R2),ACIOF5                                                   
         B     XIT                                                              
         SPACE 1                                                                
FILTOUT  MVC   LABLAREA(6),=C'FILTER'                                           
         MVC   CODEAREA(1),0(R2)                                                
         MVC   LABLAREA+7(1),GLARGS                                             
         OI    LABLAREA+7,X'F0'                                                 
         CLI   GLARGS,5            ARGUMENT 1-4 FILTER                          
         BL    GENOUT                       5=ANALYSIS 6=SUB-COMP               
         MVC   LABLAREA(8),=C'ANALYSIS'                                         
         BE    GENOUT                                                           
         MVC   LABLAREA(8),=C'SUB-COMP'                                         
         B     GENOUT                                                           
         EJECT                                                                  
*              DATE ROUTINES                                                    
         SPACE 3                                                                
YEARIN   MVI   0(R2),1                                                          
         CLC   ACIOMON,PPERIOD     BEFORE REQUEST START                         
         BL    XIT                 WILL BE GROUPED TOGETHER                     
         MVC   0(1,R2),ACIOMON                                                  
         B     XIT                                                              
         SPACE 1                                                                
YEAROUT  MVC   LABLAREA(4),=C'YEAR'                                             
         MVC   CODEAREA(4),=C'PREV'                                             
         CLI   0(R2),1                                                          
         BE    GENOUT                                                           
         MVC   WORK(1),0(R2)                                                    
         MVC   WORK+1(2),=X'0101'                                               
         GOTO1 DATCON,DMCB,(1,WORK),(20,WORK+3)                                 
         MVC   CODEAREA(4),WORK+3                                               
         B     GENOUT                                                           
         SPACE 1                                                                
MONIN    MVC   WORK(2),ACIOMON     USE MONTH OF SERVICE                         
         CLI   QPERTYPE,C'M'       IF DATES ARE NOT DETAILED                    
         BE    MONIN2                                                           
         CLI   GLARGS,1            OR IF MONTH OF SERVICE REQUESTED             
         BE    MONIN2                                                           
         CLI   ACMODE,PROCTRNS     OTHERWISE USE YEAR/MONTH FROM TRANS          
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         USING ACKEYD,R4                                                        
         MVC   WORK(2),ACKEYDTE                                                 
         SPACE 1                                                                
MONIN2   MVC   0(1,R2),WORK+1      ARG1=2 MONTH ONLY                            
         CLI   GLARGS,2                                                         
         BE    XIT                                                              
         MVI   0(R2),1                                                          
         CLC   WORK(2),PPERIOD     BEFORE REQUEST START                         
         BL    XIT                 WILL BE GROUPED TOGETHER                     
         MVC   0(2,R2),WORK                                                     
         B     XIT                                                              
         SPACE 1                                                                
MONOUT   MVC   LABLAREA(5),=C'MONTH'                                            
         CLI   GLARGS,2                                                         
         BNE   MONOUT2             OPTION JUST TO DO MONTH                      
         MVI   DUB,1                                                            
         MVC   DUB+1(1),0(R2)                                                   
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK)                                     
         MVC   CODEAREA(3),WORK                                                 
         B     GENOUT                                                           
         SPACE 1                                                                
MONOUT2  MVC   CODEAREA(5),=C'PREV.'                                            
         CLI   0(R2),1                                                          
         BE    GENOUT                                                           
         MVC   DUB(2),0(R2)                                                     
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK)                                     
         CLI   WORK+3,C'/'                                                      
         BNE   *+10                                                             
         MVC   WORK+3(2),WORK+4                                                 
         MVC   CODEAREA(5),WORK                                                 
         B     GENOUT                                                           
         SPACE 1                                                                
QUARTIN  MVI   0(R2),1                                                          
         CLC   ACIOMON,PPERIOD     BEFORE REQUEST START                         
         BL    XIT                 WILL BE GROUPED TOGETHER                     
         MVC   0(1,R2),ACIOMON                                                  
         ZIC   R1,ACIOMON+1        GET QUARTER FROM MONTH                       
         CH    R1,=H'10'           CONVERT PWOS 10-12                           
         BL    *+8                                                              
         SH    R1,=H'6'            X'10' BECOMES X'0A'                          
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         STC   R1,1(R2)                                                         
         B     XIT                                                              
         SPACE 1                                                                
QUARTOUT MVC   LABLAREA(7),=C'QUARTER'                                          
         MVC   CODEAREA(5),=C'PREV.'                                            
         CLI   0(R2),1                                                          
         BE    GENOUT                                                           
         MVC   CODEAREA,MYSPACES                                                
         UNPK  WORK(3),0(2,R2)                                                  
         MVC   CODEAREA+5(2),WORK                                               
         ZIC   R1,1(R2)                                                         
         SLL   R1,2                                                             
         LA    R1,QUARTLST(R1)                                                  
         MVC   CODEAREA(4),0(R1)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
QUARTLST DC    C'1ST.2ND.3RD.4TH.   '                                           
         SPACE 1                                                                
USECOUT  DS    0H                  USE CYCLE MMMDD/YY-MMMDD/YY                  
         GOTO1 DATCON,DMCB,(1,(R2)),(8,(R3))                                    
         MVI   8(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,3(R2)),(8,9(R3))                                  
         B     XIT                                                              
         EJECT                                                                  
*              TRANSACTION ROUTINES                                             
         SPACE 3                                                                
DATEIN   L     R4,ADTRNREC                                                      
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING ACKEYD,R4                                                        
         MVC   0(3,R2),ACKEYDTE                                                 
         CLI   GLARGS,1            ARGUMENT 1 0=TRANSACTION DATE                
         BL    XIT                            1=ACTIVITY DATE                   
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TRSTATD,R4                                                       
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,0(R2))                               
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 1                                                                
DTE2OUT  DS    0H                  COMPRESSED DATE                              
         GOTO1 DATCON,DMCB,(2,(R2)),(8,(R3))                                    
         B     XIT                                                              
         SPACE 1                                                                
DATOUT   MVC   LABLAREA(4),=C'DATE'                                             
         B     ALLDOUT                                                          
         SPACE 1                                                                
USEDOUT  MVC   LABLAREA(8),=C'USE DATE'                                         
         SPACE 1                                                                
ALLDOUT  OC    0(3,R2),0(R2)                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,0(R2)),(8,CODEAREA)                               
         B     GENOUT                                                           
         SPACE 1                                                                
CKDTOUT  MVC   LABLAREA(10),=C'CHECK DATE'                                      
         MVC   CODEAREA(8),3(R2)                                                
         B     GENOUT                                                           
         SPACE 3                                                                
WEEKIN   L     R4,ADTRNREC         WEEK COMMENCING DATES                        
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING ACKEYD,R4                                                        
         MVC   0(3,R2),ACKEYDTE    MOVE IT OUT - BUT CHECK BEFOR XIT            
         GOTO1 DATCON,DMCB,(1,ACKEYDTE),(0,WORK)                                
         DROP  R4                                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         CLI   DMCB,1              IS DATE A MONDAY                             
         BE    XIT                                                              
         ZIC   R1,DMCB             CALCULATE MONDAY'S DATE                      
         BCTR  R1,0                                                             
         LCR   R1,R1                                                            
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,(R2))                                  
         B     XIT                                                              
         SPACE 1                                                                
WEEKOUT  MVC   LABLAREA(8),=C'W/C DATE'                                         
         OC    0(3,R2),0(R2)                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,0(R2)),(8,CODEAREA)                               
         B     GENOUT                                                           
         SPACE 3                                                                
BRIN     L     R4,ADTRANS          BATCH REF                                    
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING TRANSD,R4                                                        
         MVC   0(6,R2),TRNSBTCH                                                 
         B     XIT                                                              
         SPACE 1                                                                
BROUT    MVC   LABLAREA(10),=C'BATCH REF.'                                      
         MVC   NAMEAREA(6),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
BTIN     L     R4,ADTRANS          BATCH TYPE                                   
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING TRANSD,R4                                                        
         ZIC   R1,TRNSTYPE                                                      
         EDIT  (R1),(3,0(R2))      OPTION TO GET NUMBER                         
         CLI   GLARGS,2                                                         
         BE    XIT                                                              
         DROP  R4                                                               
         BCTR  R1,0                                                             
         MH    R1,=H'15'                                                        
******** LA    R1,ACINPT(R1)                                                    
         L     RF,=A(ACINPT)                                                    
         AR    R1,RF                                                            
         MVC   0(15,R2),0(R1)                                                   
         B     XIT                                                              
         SPACE 1                                                                
BTOUT    MVC   LABLAREA(10),=C'BATCH TYPE'                                      
         MVC   NAMEAREA(15),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
REFIN    L     R4,ADTRNREC                                                      
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING ACKEYD,R4                                                        
         MVC   0(6,R2),ACKEYREF                                                 
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 1                                                                
REFOUT   MVC   LABLAREA(4),=C'REF.'                                             
         MVC   CODEAREA(6),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
SUBREF   L     R4,ADTRNREC                                                      
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING ACKEYD,R4                                                        
         ZIC   R1,ACKEYSBR                                                      
         EDIT  (R1),(3,0(R2))                                                   
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 1                                                                
SALTIN   LA    R4,SALTABLE         SALARY TYPE                                  
         MVC   0(1,R2),ACIOTYPE                                                 
         MVC   1(1,R2),ACIOTYP2                                                 
         SPACE 1                                                                
SALTIN2  CLI   0(R4),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(1,R4),ACIOTYP2                                                 
         BE    SALTIN4                                                          
         LA    R4,9(R4)                                                         
         B     SALTIN2                                                          
         SPACE 1                                                                
SALTIN4  MVC   0(8,R2),1(R4)                                                    
         B     XIT                                                              
         SPACE 1                                                                
SALTABLE DS    0C                                                               
         DC    CL9'1BASE'                                                       
         DC    CL9'2BENEFIT'                                                    
         DC    CL9'3PENSION'                                                    
         DC    CL9'4ADMIN'                                                      
         DC    CL9'5TEMP.'                                                      
         DC    CL9'6BONUS'                                                      
         DC    CL9'7OVERTIME'                                                   
         DC    CL9'TTOTAL'                                                      
         DC    X'FF',CL8'UNKNOWN'                                               
         SPACE 1                                                                
SALTOUT  MVC   LABLAREA(9),=C'SAL. TYPE'                                        
         MVC   CODEAREA(8),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
NARRIN   L     R4,ADTRANS                                                       
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING TRANSD,R4                                                        
         MVC   0(120,R2),MYSPACES  MAX LENGTH IS 120                            
         ZIC   R1,TRNSLEN                                                       
         SH    R1,=H'29'                                                        
         BM    XIT                                                              
         CH    R1,=H'120'                                                       
         BL    *+8                                                              
         LA    R1,119                                                           
         CLC   ACIOACC+1(2),=C'SJ' IF PRODUCTION                                
         BNE   NARRIN2                                                          
         CLC   ACIOWORK(2),=C'99'  BILLS HAVE CRAPPY NARRATIVE                  
         BNE   NARRIN2                                                          
         LA    R1,14               SO CUT THEM DOWN                             
         SPACE 1                                                                
NARRIN2  EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),TRNSNARR                                                 
         SPACE 1                                                                
ORDIN    L     R4,ADTRANS          LOOK FOR ORDER IN NARRATIVE                  
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING TRANSD,R4                                                        
         ZIC   R0,TRNSLEN                                                       
         SH    R0,=H'29'                                                        
         BNP   NUMIN               NO NARRATIVE - TRY NUMIN                     
         LA    R1,TRNSNARR                                                      
         SPACE 1                                                                
ORDIN2   LA    R3,4(R1)                                                         
         CLC   0(4,R1),=C'ORD='                                                 
         BE    ORDIN4                                                           
         LA    R3,6(R1)                                                         
         CLC   0(6,R1),=C'ORDER='                                               
         BE    ORDIN4                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,ORDIN2                                                        
         B     NUMIN               TRY NUMBER ELEMENT IF NOT HERE!              
         SPACE 1                                                                
ORDIN4   MVC   0(6,R2),0(R3)                                                    
         B     XIT                                                              
         SPACE 1                                                                
NUMIN    CLI   ACMODE,PROCTRNS                                                  
         BNE   TRANSEND                                                         
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACNOEL,R4                                                        
         ZIC   R1,ACNOLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),ACNO                                                     
         SPACE 1                                                                
PINTIN   L     R4,ADLVBNUM         PRODUCT INTERFACE CODE                       
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING ACNOEL,R4                                                        
         ZIC   R1,ACNOLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),ACNO                                                     
         SPACE 1                                                                
UNIQUE   L     R1,MYUNIQUE                                                      
         LA    R1,1(R1)                                                         
         ST    R1,MYUNIQUE                                                      
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         SPACE 1                                                                
MYUNIQUE DC    F'0'                                                             
         EJECT                                                                  
*              FIELD FORM OTHER ELEMENT                                         
         SPACE 3                                                                
*              AGUMENT 1           1=FREE FORM NUMBER                           
*                                  2=PROFILE                                    
*                                  3=MEDIA MONTH OF SERVOCE                     
*                                  4=NETWORK                                    
         SPACE 1                                                                
OTHERIN  CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,ACOTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACOTHERD,R4                                                      
         CLI   GLARGS,2                                                         
         BL    OTHER1                                                           
         BE    OTHER2                                                           
         CLI   GLARGS,4                                                         
         BL    OTHER3                                                           
         BE    OTHER4                                                           
         SPACE 1                                                                
OTHER1   MVC   0(9,R2),ACOTNUM     FREE FROM NUMBER                             
         B     XIT                                                              
         SPACE 1                                                                
OTHER2   MVC   0(4,R2),ACOTPROF    PROFILE                                      
         B     XIT                                                              
         SPACE 1                                                                
OTHER3   MVC   0(2,R2),ACOTDATE    MEDIA MONTH OF SERVICE                       
         B     XIT                                                              
         SPACE 1                                                                
OTHER4   CLI   ACOTLEN,X'19'                                                    
         BL    XIT                                                              
         MVC   0(4,R2),ACOTNET     NETWORK                                      
         B     XIT                                                              
         EJECT                                                                  
*              PAYABLE RELATED FIELDS                                           
         SPACE 3                                                                
REPIN    CLI   ACMODE,PROCTRNS     REP ON PAYABLE                               
         BNE   XIT                                                              
         MVC   0(3,R2),REPSAVE                                                  
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSTYPE,X'22'                                                   
         BE    REPIN2                                                           
         CLI   TRNSTYPE,X'32'                                                   
         BNE   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
REPIN2   MVC   0(3,R2),ACIOACC+3                                                
         MVC   REPSAVE,0(R2)                                                    
         B     XIT                                                              
         SPACE 1                                                                
STATIN   CLI   ACMODE,PROCTRNS     STATION ON PAYABLE                           
         BNE   XIT                                                              
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         MVC   0(7,R2),STATSAVE                                                 
         LA    R1,ACIOACC+3                                                     
         CLI   TRNSTYPE,X'21'                                                   
         BE    STATIN2                                                          
         LA    R1,ACIOCON+3                                                     
         CLI   TRNSTYPE,X'22'                                                   
         BNE   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
STATIN2  MVC   0(4,R2),0(R1)       EDIT CALL LETTERS                            
         MVC   4(3,R2),MYSPACES                                                 
         CLI   4(R1),X'41'                                                      
         BL    STATIN4                                                          
         MVC   4(3,R2),=C'-TV'                                                  
         CLI   4(R1),C'T'                                                       
         BE    STATIN4                                                          
         MVC   4(3,R2),=C'-AM'                                                  
         CLI   4(R1),C'A'                                                       
         BE    STATIN4                                                          
         MVC   4(3,R2),=C'-FM'                                                  
         CLI   4(R1),C'F'                                                       
         BE    STATIN4                                                          
         MVC   4(3,R2),MYSPACES                                                 
         SPACE 1                                                                
STATIN4  MVC   STATSAVE,0(R2)                                                   
         B     XIT                                                              
         SPACE 1                                                                
PUBIN    CLI   ACMODE,PROCTRNS     PUBLICATION ON PAYABLE                       
         BNE   XIT                                                              
         MVC   0(11,R2),PUBSAVE                                                 
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         LA    R1,ACIOACC+3                                                     
         CLI   TRNSTYPE,X'31'                                                   
         BE    PUBIN2                                                           
         LA    R1,ACIOCON+1                                                     
         CLI   TRNSTYPE,X'32'                                                   
         BNE   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
PUBIN2   MVC   0(11,R2),0(R1)      EDIT CALL LETTERS                            
         MVC   PUBSAVE,0(R2)                                                    
         B     XIT                                                              
         SPACE 1                                                                
REPSAVE  DC    CL3' '                                                           
PUBSAVE  DC    CL11' '                                                          
STATSAVE DC    CL7' '                                                           
         EJECT                                                                  
*              ANALYSIS OF TRANSACTION NARRATIVE                                
         SPACE 3                                                                
*              ARGUMENTS           1  1=CHECK NUMBER                            
*                                     2=CHECK DATE                              
*                                     3=CHECK AMOUNT                            
*                                     4=CHECK CASH ACCOUNT                      
         SPACE 1                                                                
CHECK    L     R4,ADTRANS                                                       
         CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         USING TRANSD,R4                                                        
         CLI   TRNSTYPE,X'81'      X'81'=CASH DISBURSMENT                       
         BNE   XIT                                                              
         CLI   GLARGS,2                                                         
         BL    CKNUM                                                            
         BE    CKDATE                                                           
         CLI   GLARGS,4                                                         
         BL    CKAMT                                                            
         BE    CKCASHAC                                                         
         B     XIT                                                              
         SPACE 1                                                                
CKNUM    MVC   0(6,R2),TRNSNARR                                                 
         B     XIT                                                              
         SPACE 1                                                                
CKDATE   LA    R1,MYMONTHS                                                      
         LA    RE,1                                                             
         LA    R0,12                                                            
         SPACE 1                                                                
CKDATE2  DS    0H                  MATCH ON MONTH IN NARR                       
*&&US*&& CLC   0(3,R1),TRNSNARR+6                                               
*&&UK*&& CLC   0(3,R1),TRNSNARR+8                                               
         BE    CKDATE4                                                          
         LA    R1,3(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CKDATE2                                                       
         SPACE                                                                  
CKDATE4  STC   RE,1(R2)                                                         
*&&US*&& PACK  DUB,TRNSNARR+9(2)                                                
*&&UK*&& PACK  DUB,TRNSNARR+6(2)                                                
         CVB   R1,DUB                                                           
         STC   R1,2(R2)                                                         
*&&US*&& PACK  DUB,TRNSNARR+12(2)                                               
*&&UK*&& PACK  DUB,TRNSNARR+11(2)                                               
         CVB   R1,DUB                                                           
         STC   R1,0(R2)                                                         
         MVC   3(8,R2),TRNSNARR+6                                               
         B     XIT                                                              
         SPACE 1                                                                
CKAMT    MVC   0(11,R2),TRNSNARR+14                                             
         B     XIT                                                              
         SPACE 1                                                                
CKCASHAC MVC   0(14,R2),TRNSNARR+27                                             
         B     XIT                                                              
         EJECT                                                                  
*              BUCKET HANDLING                                                  
         SPACE 3                                                                
*              ARGUMENTS           1  D=DEBIT C=CREDIT B=BOTH                   
*                                     P=PREVIOUS A=ALL E=EITHER                 
*                                  2  BUCKET TYPE OR X'00'                      
*                                  7-12  ARGUMENTS TO ARGFILT                   
*                                  13-16 START/END YEAR/MONTH PWOS              
         SPACE 1                                                                
BUCKET   ZAP   DUB,=P'0'           BUCKET                                       
         CLI   ACMODE,PROCHIST                                                  
         BE    BUCKET2                                                          
         CLI   ACMODE,PROCTRNS     IF THIS IS A TRANSACTION                     
         BNE   BUCKEND                                                          
         CLI   FRCETRNS,C'Y'       AND WE WERE FORCED TO READ THEM              
         BNE   BUCKEND                 UNLESS THIS IS A                         
         CLI   GLARGS,C'P'             PREVIOUS BALANCE TYPE                    
         BE    BUCKEND                                                          
         CLI   GLARGS,C'A'             OR ALL BALANCE TYPE                      
         BE    BUCKEND                                                          
         B     TRANS               GO AND USE TRANSACTION ROUTINE               
         SPACE 1                                                                
BUCKET2  CLI   FRCETRNS,C'Y'       WERE WE FORCED TO READ TRANSACTIONS?         
         BNE   BUCKET3             THEN I AM ONLY INTERESTED IF...              
         CLI   GLARGS,C'P'         PREVIOUS BALANCE TYPE                        
         BE    BUCKET3                                                          
         CLI   GLARGS,C'A'         OR ALL BALANCE TYPE                          
         BE    BUCKET3                                                          
         B     BUCKEND                                                          
         SPACE 1                                                                
BUCKET3  LA    R1,PPERIOD          DEFAULT IS REQUEST PERIOD                    
         OC    GLARGS+12(4),GLARGS+12                                           
         BZ    *+8                                                              
         LA    R1,GLARGS+12        ELSE PICK UP COLUMN S/E                      
         CLI   GLARGS,C'P'         PREVIOUS BALANCE TYPE                        
         BE    BUCKET4                                                          
         CLI   GLARGS,C'A'         OR ALL BALANCE TYPE                          
         BNE   BUCKET8                                                          
         SPACE 1                                                                
BUCKET4  MVC   BALDATES,0(R1)      BALANCE TYPES (A & P)                        
         XC    BALDATES(2),BALDATES                                             
         CLI   GLARGS,C'P'         TYPE P (BBF) PICKS UP ALL                    
         BNE   *+10                             BEFORE REQUEST START            
         MVC   BALDATES+2(2),BEFSTART                                           
         CLI   ACIOBFT,C'P'        FOR P&L TYPE LEDGERS/ACCOUNTS                
         BNE   BUCKET6                                                          
         L     R1,ADATES                                                        
         MVC   BALDATES(2),240(R1) START IS START OF FISCAL YEAR                
         SPACE 1                                                                
BUCKET6  LA    R1,BALDATES                                                      
         SPACE 1                                                                
BUCKET8  CLC   ACIOMON,0(R1)       CHECK DATE NOT BEFORE START                  
         BL    BUCKEND                                                          
         CLC   ACIOMON,2(R1)       CHECK DATE NOT AFTER END                     
         BH    BUCKEND                                                          
         CLI   GLARGS+1,0          SECOND ARG CAN BE BUCKET TYPE                
         BNE   BUCK2                                                            
         CLI   ACIOTYPE,X'40'      IF NOT REJECT BUCKETS WITH TYPE              
         BH    BUCKEND                                                          
         B     BUCK4                                                            
         SPACE 1                                                                
BUCK2    CLC   ACIOTYPE,GLARGS+1   ELSE CHECK TYPE MATCHES                      
         BNE   BUCKEND                                                          
         CLI   GLARGS+2,0          THIRD ARG CAN BE BUCKET TYPE 2               
         BE    BUCK4                                                            
         CLC   ACIOTYP2,GLARGS+2   SO CHECK THIS AS WELL                        
         BNE   BUCKEND                                                          
         SPACE 1                                                                
BUCK4    ZAP   DUB,ACIODR                                                       
         CLI   GLARGS,C'D'         DEBITS SELECTED                              
         BE    BUCKEND                                                          
         ZAP   DUB,ACIOCR                                                       
         CLI   GLARGS,C'C'         CREDITS SELECTED                             
         BE    BUCKEND                                                          
         ZAP   DUB,ACIODR                                                       
         AP    DUB,ACIOCR                                                       
         CLI   GLARGS,C'E'         EITHER SELECTED                              
         BE    BUCKEND                                                          
         ZAP   DUB,ACIODR          BOTH SELECTED                                
         SP    DUB,ACIOCR                                                       
         SPACE 1                                                                
BUCKEND  BAS   RE,BUCKROUT         MAY BE SPECIAL ROUTINES                      
         BAS   RE,ARGFILT          MAY BE ARGUMENT FILTERS                      
         ZAP   0(8,R2),DUB                                                      
         CLI   HISTACSW,0          SET THAT WE HAVE BEEN HERE ONCE              
         BNE   *+8                                                              
         MVI   HISTACSW,1                                                       
         CP    DUB,=P'0'           AND SET TO 2 IF ANY SIGNIFICANCE             
         BE    XIT                                                              
         MVI   HISTACSW,2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              EXTRA BUCKET ROUTINES                                            
         SPACE 3                                                                
*              ARGUMENT 3          1-10 ARE P&L                                 
         SPACE 1                                                                
BUCKROUT NTR1                                                                   
         CLI   GLARGS+2,0                                                       
         BE    XIT                                                              
         CLI   GLARGS+2,10                                                      
         BH    XIT                                                              
         ZIC   RF,GLARGS+2                                                      
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     BUCKR0(RF)                                                       
         SPACE 1                                                                
BUCKR0   B     BUCKR1                                                           
         B     BUCKR2                                                           
         B     BUCKR3                                                           
         B     BUCKR4                                                           
         B     BUCKR5                                                           
         B     BUCKR6                                                           
         B     BUCKR7                                                           
         B     BUCKR8                                                           
         B     BUCKR9                                                           
         B     BUCKR10                                                          
         SPACE 1                                                                
BUCKR1   CLC   ACIOCON+1(2),=C'11'   BILLINGS                                   
         BE    XIT                                                              
         B     BUCKNO                                                           
         SPACE 1                                                                
BUCKR2   CLC   ACIOCON+1(2),=C'12'   INCOME                                     
         BE    XIT                                                              
         B     BUCKNO                                                           
         SPACE 1                                                                
BUCKR3   CLC   ACIOCON+1(2),=C'13'   EXPENSE                                    
         BE    BUCKFLIP                                                         
         B     BUCKNO                                                           
         SPACE 1                                                                
BUCKR4   CLC   ACIOCON+1(2),=C'14'   TIME COSTS                                 
         BE    BUCKFLIP                                                         
         B     BUCKNO                                                           
         SPACE 1                                                                
BUCKR5   CLC   ACIOCON+1(2),=C'15'   OVERHEADS                                  
         BE    BUCKFLIP                                                         
         B     BUCKNO                                                           
         SPACE 1                                                                
BUCKR6   CLC   ACIOCON+1(2),=C'13'   DIRECT COSTS (EXPENSE + TIME)              
         BE    BUCKFLIP                                                         
         CLC   ACIOCON+1(2),=C'14'                                              
         BE    BUCKFLIP                                                         
         B     BUCKNO                                                           
         SPACE 1                                                                
BUCKR7   CLC   ACIOCON+1(2),=C'12'   MARGIN (INCOME - DIRECT)                   
         BE    XIT                                                              
         CLC   ACIOCON+1(2),=C'13'                                              
         BE    XIT                                                              
         CLC   ACIOCON+1(2),=C'14'                                              
         BE    XIT                                                              
         B     BUCKNO                                                           
         SPACE 1                                                                
BUCKR8   CLC   ACIOCON+1(2),=C'13'   TOTAL COSTS (13-17)                        
         BL    BUCKNO                                                           
         CLC   ACIOCON+1(2),=C'17'                                              
         BH    BUCKNO                                                           
         B     BUCKFLIP                                                         
         SPACE 1                                                                
BUCKR9   CLC   ACIOCON+1(2),=C'12'   P&L (INCOME - COSTS)                       
         BE    XIT                                                              
         CLC   ACIOCON+1(2),=C'13'                                              
         BL    BUCKNO                                                           
         CLC   ACIOCON+1(2),=C'17'                                              
         BH    BUCKNO                                                           
         B     XIT                                                              
         SPACE 1                                                                
BUCKR10  CLC   ACIOCON+1(2),=C'11'   P&L SPECIAL                                
         BE    XIT                                                              
         CLC   ACIOCON+1(2),=C'12'                                              
         BE    XIT                                                              
         BH    BUCKNO                                                           
         B     XIT                                                              
         SPACE 1                                                                
BUCKNO   ZAP   DUB,=P'0'                                                        
         B     XIT                                                              
         SPACE 1                                                                
BUCKFLIP MP    DUB,=P'-1'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              MAY HAVE FILTERS IN THE ARGUMENTS                                
         SPACE 3                                                                
*              ARGUMENTS           UP TO 3 2 BYTE FILTERS                       
*                                  BYTE 1 IS FILTER TYPE                        
*                                  1-4=FILTERS 1-4                              
*                                  5=CONTRA LEDGER 6=MEDIA 7=TIME TYPE          
*                                  8=MGROUP 9=OGROUP 10=WGROUP                  
*                                  BYTE 2 IS FILTER VALUE                       
*                                  20=ACCOUNT, IN WHICH CASE                    
*                                  BYTE 2 IS COLUMN NUMBER                      
*                                  X'40' BIT OFF NEGATIVE                       
         SPACE 1                                                                
ARGFILT  NTR1                                                                   
         LA    R2,GLARGS+6                                                      
         LA    R0,3                                                             
         SPACE 1                                                                
AF2      CLI   0(R2),1                                                          
         BL    XIT                                                              
         LA    R3,ACIOFILT                                                      
         BE    AF4                                                              
         LA    R3,ACIOFILT+1                                                    
         CLI   0(R2),3                                                          
         BL    AF4                                                              
         LA    R3,ACIOFILT+2                                                    
         BE    AF4                                                              
         LA    R3,ACIOFILT+3                                                    
         CLI   0(R2),5                                                          
         BL    AF4                                                              
         LA    R3,ACIOCON+2                                                     
         BE    AF4                                                              
         LA    R3,ACIOMED                                                       
         CLI   0(R2),7                                                          
         BL    AF4                                                              
         LA    R3,ACIOTTYP                                                      
         BE    AF4                                                              
         LA    R3,ACIOMG                                                        
         CLI   0(R2),9                                                          
         BL    AF4                                                              
         LA    R3,ACIOOG                                                        
         BE    AF4                                                              
         LA    R3,ACIOWG                                                        
         CLI   0(R2),11                                                         
         BL    AF4                                                              
         LA    R3,ACIORTYP                                                      
         BE    AF4                                                              
         CLI   0(R2),20                                                         
         BL    AFNO                                                             
         BAS   RE,BIGFILT          LONGER FILTERS                               
         B     AFNEXT                                                           
         SPACE 1                                                                
AF4      TM    1(R2),X'40'                                                      
         BNO   AF6                                                              
         CLC   1(1,R2),0(R3)       NOW CHECK FILTER                             
         BE    AFNEXT                                                           
         B     AFNO                                                             
         SPACE 1                                                                
AF6      MVC   FILTBYTE,1(R2)      CHECK A NEGATIVE FILTER                      
         OI    FILTBYTE,X'40'                                                   
         CLC   FILTBYTE,0(R3)                                                   
         BE    AFNO                                                             
         B     AFNEXT                                                           
         SPACE 1                                                                
AFNO     ZAP   DUB,=P'0'                                                        
         B     XIT                                                              
         SPACE 1                                                                
AFNEXT   LA    R2,2(R2)                                                         
         BCT   R0,AF2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              FILTERING OF LONGER EXPRESSIONS                                  
         SPACE 3                                                                
BIGFILT  NTR1                                                                   
         ZIC   RF,0(R2)            PICK UP SCHEME NO. (20+)                     
         SH    RF,=H'20'                                                        
         SLL   RF,8                AND INDEX INTO FILTER SCHEME                 
         ZIC   R1,1(R2)            PICK UP COLUMN NUMBER                        
         BCTR  R1,0                                                             
         SLL   R1,4                AND INDEX INTO COLUMN                        
         AR    R1,RF               ADD THESE TOGETHER                           
         A     R1,AACCFILT         ADDRESS FILTERED DETAILS                     
         SPACE 1                                                                
*                                  NOW POINT RE TO DATA TO BE FILTERED          
*                                  AND RF=L'DATA TO BE FILTERED                 
         LA    RE,ACIOACC+3        20=ACCOUNT                                   
         LA    RF,12                                                            
         CLI   0(R2),20                                                         
         BE    BF8                                                              
         LA    RE,ACIOCON+1        21=CONTRA U/L/ACCOUNT                        
         LA    RF,14                                                            
         CLI   0(R2),21                                                         
         BE    BF8                                                              
         LA    RE,ACIOWORK         22=WORK CODE                                 
         LA    RF,2                                                             
         CLI   0(R2),22                                                         
         BE    BF8                                                              
         LA    RE,ACIOBT           24=BATCH TYPE                                
         LA    RF,2                                                             
         CLI   0(R2),24                                                         
         BE    BF8                                                              
         LA    RE,ACIONOFF         25=NEW OFFICE CODE                           
         LA    RF,2                                                             
         CLI   0(R2),25                                                         
         BE    BF8                                                              
         L     R3,ADATTR           23=ATTRIBUTE                                 
         LTR   R3,R3                                                            
         BZ    AFNO                                                             
         USING ACAPD,R3                                                         
         LA    RE,ACAPMLEN         A(MINI ELEMENTS)                             
         ZIC   R0,ACAPMIN          N'MINI ELEMENTS                              
         DROP  R3                                                               
         USING ACAPMLEN,RE                                                      
         MVC   WORK(2),0(R1)       GET REQUEST U/L INTO WORK(2)                 
         OI    WORK,X'40'                                                       
         SPACE 1                                                                
BF2      ZIC   RF,0(RE)            LENGTH OF THIS ELEMENT                       
         SH    RF,=H'2'            (ACTUAL DATA WILL BE 2 LESS)                 
         CLC   WORK(2),ACAPACCT    FIND ATTRIBUTE FOR THIS U/L                  
         BE    BF4                                                              
         LA    RE,2(RE,RF)         BUMP TO NEXT MINI                            
         BCT   R0,BF2                                                           
         B     AFNO                CAN'T FIND IT - SO TREAT AS MISS             
         SPACE 1                                                                
BF4      LA    RE,ACAPACCT         FOUND SO ADDRESS U/L/ACCOUNT                 
         DROP  RE                                                               
         SPACE 1                                                                
BF8      TM    0(R1),X'40'         SEE IF THIS IS NEG OR POS FILTER             
         BO    BF10                                                             
         MVC   WORK(16),0(R1)                                                   
         OI    WORK,X'40'                                                       
         LA    R1,WORK                                                          
         B     BF14                                                             
         SPACE 1                                                                
BF10     CLI   0(R1),C'A'          POSITIVE FILTER TEST                         
         BL    BF12                                                             
         CLC   0(1,R1),0(RE)                                                    
         BNE   AFNO                                                             
         SPACE 1                                                                
BF12     LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,BF10                                                          
         B     XIT                                                              
         SPACE 1                                                                
BF14     CLI   0(R1),C'A'         NEGATIVE FILTER TEST                          
         BL    BF16                                                             
         CLC   0(1,R1),0(RE)                                                    
         BNE   XIT                                                              
         SPACE 1                                                                
BF16     LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,BF14                                                          
         B     AFNO                                                             
         EJECT                                                                  
*              TRANSACTION HANDLING                                             
         SPACE 3                                                                
*              ARGUMENTS           1  D=DEBIT C=CREDIT B=BOTH A=AMOUNT          
*                                  2  SUB TYPE OR X'00'                         
*                                  7-12  ARGUMENTS TO ARGFILT                   
*                                  13-16 START/END YEAR/MONTH PWOS              
         SPACE 1                                                                
TRANS    BAS   RE,TRNCHECK         CHECK MODE AND DATES                         
         BNE   TRANSEND                                                         
         CLC   ACIOSBTY,GLARGS+1   CHECK TYPE MATCHES V SECOND ARG              
         BNE   TRANSEND                                                         
         CLI   ACSUBMOD,PROC4B                                                  
         BE    TRANSEND                                                         
         SPACE 1                                                                
TRANS2   ZAP   DUB,ACIODR                                                       
         CLI   GLARGS,C'D'         DEBITS SELECTED                              
         BE    TRANSEND                                                         
         ZAP   DUB,ACIOCR                                                       
         CLI   GLARGS,C'C'         CREDITS SELECTED                             
         BE    TRANSEND                                                         
         ZAP   DUB,ACIODR                                                       
         SP    DUB,ACIOCR                                                       
         CLI   GLARGS,C'B'         BALANCE SELECTED                             
         BE    TRANSEND                                                         
         ZAP   DUB,ACIODR                                                       
         AP    DUB,ACIOCR                                                       
         CLI   GLARGS,C'E'         EITHER SELECTED                              
         BE    TRANSEND                                                         
         ZAP   DUB,=P'0'                                                        
         B     TRANSEND                                                         
         SPACE 1                                                                
CVDEND   L     R1,0(R2)                                                         
         CVD   R1,DUB                                                           
         SPACE 1                                                                
TRANSEND BAS   RE,ARGFILT          MAY BE ARGUMENT FILTERS                      
         ZAP   0(8,R2),DUB                                                      
         B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR MODE AND DATE CHECKING                   
         SPACE 3                                                                
TRNCHECK NTR1                                                                   
         ZAP   DUB,=P'0'                                                        
         B     ALLCHECK                                                         
         SPACE 1                                                                
SUBCHECK NTR1                                                                   
         ZAP   DUB,=P'0'           USED FOR SUBSIDIARY ELEMENTS                 
         CLI   ACIOSBTY,0          NOT INTERESTED WHEN SUB CASH                 
         BNE   NOGOOD                                                           
         SPACE 1                                                                
ALLCHECK CLI   ACMODE,PROCTRNS                                                  
         BNE   NOGOOD                                                           
         BAS   RE,DATEFILT                                                      
         BNE   NOGOOD                                                           
         B     ITSFINE                                                          
         EJECT                                                                  
*              DATE FILTERING ROUTINE                                           
         SPACE 3                                                                
DATEFILT NTR1                                                                   
*****    L     R2,ADTRNREC         USING THE TRANSACTION DATE                   
*****    USING ACKEYD,R2                                                        
         OC    GLARGS+12(4),GLARGS+12    WERE ARGS SPECIFIED?                   
         BNZ   DFDETAIL                                                         
         SPACE 1                                                                
         CLC   ACIODTE,QTRASTR     NO SO TEST TRANSACTION DATE                  
         BL    NOGOOD                                                           
         CLC   ACIODTE,QTRAEND                                                  
         BH    NOGOOD                                                           
         CLC   ACIOMON,QMOSSTR           AND MONTH OF SERVICE                   
         BL    NOGOOD                                                           
         CLC   ACIOMON,QMOSEND                                                  
         BH    NOGOOD                                                           
         B     ITSFINE                                                          
         SPACE 1                                                                
DFDETAIL CLC   GLARGS+14(1),GLARGS+12  TEST PACKED OR COMPRESSED DATES          
         BL    DFCOMP              END < START = 'COMPRESSED' INDICATO          
*DFDETAIL CLI   GLARGS+12,X'99'     ARGS ARE UNCOMPRESSED                       
*         BH    DFCOMP                   DEAL WITH THIS BELOW                   
         CLI   QPERTYPE,C'T'       CHECK WHICH DATE TO CHECK AGAINST            
         BNE   DFMOS                                                            
         CLC   ACIODTE(2),GLARGS+12    TEST TRANSACTION YEAR/MONTH              
         BL    NOGOOD                                                           
         CLC   ACIODTE(2),GLARGS+14                                             
         BH    NOGOOD                                                           
         B     ITSFINE                                                          
         SPACE 1                                                                
DFMOS    CLC   ACIOMON,GLARGS+12   MONTH OF SERVICE ARGUMENTS                   
         BL    NOGOOD                                                           
         CLC   ACIOMON,GLARGS+14                                                
         BH    NOGOOD                                                           
         B     ITSFINE                                                          
         SPACE 1                                                                
DFCOMP   OI    GLARGS+14,X'80'     RESTORE MISSING BIT                          
         CLC   ACIOCDAT,GLARGS+12  COMPRESSED ARGUMENTS                         
         BL    NOGOOD                                                           
         CLC   ACIOCDAT,GLARGS+14                                               
         BH    NOGOOD                                                           
         B     ITSFINE                                                          
*****    DROP  R2                                                               
         EJECT                                                                  
*              X'1A' ELEMENTS - MEDIA TRANSFER                                  
         SPACE 3                                                                
*              ARGUMENT 1          1=GST AMOUNT                                 
*                                  2='TRUE PAYABLE'                             
         SPACE 1                                                                
IN1A     CLI   GLARGS,1            FOR GST AMOUNTS                              
         BNE   IN1AB                                                            
         CLI   ACIOACC+2,C'R'      CHECK LEDGER                                 
         BE    IN1AB                                                            
         CLI   ACIOACC+2,C'I'                                                   
         BE    IN1AB                                                            
         MVI   GLARGS,C'B'         ELSE USE BUCKET ROUTINES                     
         MVI   GLARGS+1,C'T'       FOR TYPE T DATA                              
         B     BUCKET                                                           
         SPACE 1                                                                
IN1AB    ZAP   0(8,R2),=P'0'                                                    
         BAS   RE,SUBCHECK                                                      
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'1A'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACMTD,R4                                                         
         CLI   GLARGS,1                                                         
         BE    IN1A1                                                            
         CLI   GLARGS,2                                                         
         BE    IN1A2                                                            
         SPACE 1                                                                
IN1A1    MVC   0(4,R2),ACMTVAT                                                  
         B     CVDEND                                                           
         SPACE 1                                                                
IN1A2    MVC   0(4,R2),ACMTNET                                                  
         B     CVDEND                                                           
         EJECT                                                                  
*              GST AMOUNTS AND BASIS                                            
         SPACE 3                                                                
*              ARGUMENT 1          A=GST AMOUNT                                 
*                                  B=GST BASIS                                  
         SPACE 1                                                                
GSTIN    ZAP   0(8,R2),=P'0'                                                    
         BAS   RE,SUBCHECK                                                      
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,0                                                         
         ZAP   GSTVAT,=P'0'                                                     
         ZAP   GSTTAX,=P'0'                                                     
         ZAP   GSTNET,=P'0'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
GSTIN2   BAS   RE,NEXTEL                                                        
         BNE   GSTIN8                                                           
         CLI   0(R4),X'1A'                                                      
         BE    GSTIN4                                                           
         CLI   0(R4),X'50'                                                      
         BE    GSTIN6                                                           
         B     GSTIN2                                                           
         SPACE 1                                                                
         USING ACMTD,R4                                                         
GSTIN4   MVC   DUB,ACMTVAT         LOOK FOR GST TAX IN VAT FIELD                
         L     R1,DUB                                                           
         CVD   R1,GSTVAT                                                        
         B     GSTIN2                                                           
         SPACE 1                                                                
         USING TRCASHD,R4                                                       
GSTIN6   CLI   TRCSTYPE,C'T'       LOOK FOR GST TAX                             
         BNE   *+10                                                             
         ZAP   GSTTAX,TRCSAMNT                                                  
         CLI   TRCSTYPE,C'N'            AND GST NET                             
         BNE   *+10                                                             
         ZAP   GSTNET,TRCSAMNT                                                  
         B     GSTIN2                                                           
         SPACE 1                                                                
GSTIN8   CLI   ACIOACC+2,C'R'      CHECK LEDGER                                 
         BE    GSTRECL                                                          
         CLI   ACIOACC+2,C'I'                                                   
         BE    GSTRECL                                                          
         CLI   ACIOACC+2,C'G'                                                   
         BE    GSTTAXL                                                          
         SPACE 1                                                                
GSTPAYL  ZAP   0(8,R2),GSTTAX      PAYABLE LEDGERS                              
         CLI   GLARGS,C'A'                                                      
         BE    XIT                                                              
         ZAP   0(8,R2),ACIOCR      BASIS=CR-TAX                                 
         SP    0(8,R2),GSTTAX                                                   
         B     XIT                                                              
         SPACE 1                                                                
GSTRECL  ZAP   0(8,R2),GSTVAT      RECEIVABLE LEDGERS                           
         CLI   GLARGS,C'A'                                                      
         BE    XIT                                                              
         ZAP   0(8,R2),ACIODR      BASIS=DR-TAX                                 
         SP    0(8,R2),GSTVAT                                                   
         B     XIT                                                              
         SPACE 1                                                                
GSTTAXL  ZAP   0(8,R2),ACIODR      TAX LEDGER                                   
         SP    0(8,R2),ACIOCR                                                   
         CLI   GLARGS,C'A'                                                      
         BE    XIT                                                              
         ZAP   0(8,R2),GSTNET      BASIS=GSTNET                                 
         B     XIT                                                              
         SPACE 1                                                                
         DS    0D                                                               
GSTVAT   DC    PL8'0'                                                           
GSTTAX   DC    PL8'0'                                                           
GSTNET   DC    PL8'0'                                                           
         EJECT                                                                  
*              BALANCE ELEMENTS                                                 
         SPACE 3                                                                
*              ARGUMENT 1          D=FILL IF BALANCE IS DEBIT                   
*                                  C=FILL IF BALANCE IS CREDIT                  
         SPACE 1                                                                
IN32     ZAP   0(8,R2),=P'0'                                                    
         CLI   ACMODE,PROCACC      ONLY HANDLE AT PROCACC TIME                  
         BNE   XIT                                                              
         L     R4,ADACCREC                                                      
         MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACBALD,R4                                                        
         ZAP   DUB,ACBLFRWD                                                     
         AP    DUB,ACBLDR                                                       
         SP    DUB,ACBLCR                                                       
         BZ    XIT                                                              
         BM    IN32CRED                                                         
         SPACE 1                                                                
         CLI   GLARGS,C'D'         BALANCE POSITIVE                             
         BNE   XIT                 TAKE IF DEBITS NEEDED                        
         ZAP   0(8,R2),DUB                                                      
         B     XIT                                                              
         SPACE 1                                                                
IN32CRED CLI   GLARGS,C'C'         BALANCE NEGATIVE                             
         BNE   XIT                 TAKE IF CREDITS NEEDED                       
         MP    DUB,=P'-1'                                                       
         ZAP   0(8,R2),DUB                                                      
         B     XIT                                                              
         EJECT                                                                  
*              PERSONNEL - X'40' ELEMENTS                                       
         SPACE 3                                                                
*              ARGUMENT 1          1=EFFECTIVE DATE                             
*                                  2=RATE                                       
*                                  3=HOURS                                      
*                                  4=TYPE OF TIME                               
*                                  5=RATE X HOURS = COST                        
         SPACE 1                                                                
IN40     CLI   GLARGS,5                                                         
         BNE   *+10                                                             
         ZAP   0(8,R2),=P'0'                                                    
         BAS   RE,SUBCHECK                                                      
         BNE   XIT                                                              
         CLI   ACSUBMOD,PROC4B                                                  
         BE    XIT                                                              
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACPERSD,R4                                                       
         CLI   GLARGS,1                                                         
         BE    IN401                                                            
         CLI   GLARGS,2                                                         
         BE    IN402                                                            
         CLI   GLARGS,3                                                         
         BE    IN403                                                            
         CLI   GLARGS,4                                                         
         BE    IN404                                                            
         CLI   GLARGS,5                                                         
         BE    IN405                                                            
         B     XIT                                                              
         SPACE 1                                                                
IN401    MVC   0(3,R2),ACPSSTRT    EFFECTIVE START DATE                         
         B     XIT                                                              
         SPACE 1                                                                
IN402    OC    ACPSRATE,ACPSRATE             HOURLY RATE                        
         BZ    XIT                                                              
         EDIT  (P4,ACPSRATE),(7,0(R2)),2                                        
         TM    ACPSSTAT,ACPSADJ    ADJUSTMENT?                                  
         BNO   XIT                                                              
         MVI   7(R2),C'*'          SUFFIX OF *                                  
         B     XIT                                                              
         SPACE 1                                                                
IN403    BAS   RE,GETHOURS         HOURS                                        
         B     XIT                                                              
         SPACE 1                                                                
IN404    TM    ACPSSTAT,ACPSBIL    TYPE OF TIME                                 
         BNO   *+8                                                              
         MVI   0(R2),C'B'                                                       
         TM    ACPSSTAT,ACPSNOT                                                 
         BNO   *+8                                                              
         MVI   0(R2),C'N'                                                       
         TM    ACPSSTAT,ACPSRTE                                                 
         BNO   *+8                                                              
         MVI   0(R2),C'R'                                                       
         B     XIT                                                              
         SPACE 1                                                                
IN405    OC    ACPSHOUR,ACPSHOUR   COST = HOURS X RATE                          
         BZ    XIT                                                              
         OC    ACPSRATE,ACPSRATE                                                
         BZ    XIT                                                              
         BAS   RE,GETHOURS                                                      
         L     R1,0(R2)                                                         
         ZAP   DUB,ACPSRATE                                                     
         CVB   R0,DUB                                                           
         MR    R0,R0                                                            
         D     R0,=F'50'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,0(R2)                                                         
         B     XIT                                                              
         SPACE 1                                                                
GETHOURS NTR1                                                                   
         ZAP   DUB,ACPSHOUR        RETURN HOURS IN A(R2)                        
         BAS   RE,ARGFILT                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PAYABLES - X'46' ELEMENTS                                        
         SPACE 3                                                                
*              ARGUMENT 1          1=CASH DISCOUNT                              
*                                  2=CLIENT                                     
*                                  3=PRODUCT                                    
*                                  4=INVOICE                                    
*                                  5=PERIOD                                     
*                                  6=TYPE                                       
*                                  7=EST                                        
*                                  8=CLEARED                                    
*                                  9=PAID                                       
*                                  10=UNPAID                                    
         SPACE 1                                                                
IN46     BAS   RE,SUBCHECK                                                      
         BNE   TRANSEND                                                         
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'46'                                                     
         BAS   RE,GETEL                                                         
         BE    IN46B                                                            
         LA    R4,SAVE46                                                        
         B     IN46D                                                            
         SPACE 1                                                                
SAVE46   DC    XL85'00'                                                         
         SPACE 1                                                                
IN46B    MVC   SAVE46,0(R4)                                                     
         SPACE 1                                                                
         USING TRPAYD,R4                                                        
IN46D    CLI   GLARGS,1                                                         
         BE    IN461                                                            
         CLI   GLARGS,2                                                         
         BE    IN462                                                            
         CLI   GLARGS,3                                                         
         BE    IN463                                                            
         CLI   GLARGS,4                                                         
         BE    IN464                                                            
         CLI   GLARGS,5                                                         
         BE    IN465                                                            
         CLI   GLARGS,6                                                         
         BE    IN466                                                            
         CLI   GLARGS,7                                                         
         BE    IN467                                                            
         CLI   GLARGS,8                                                         
         BE    IN468                                                            
         CLI   GLARGS,9                                                         
         BE    IN469                                                            
         CLI   GLARGS,10                                                        
         BE    IN4610                                                           
         B     XIT                                                              
         SPACE 1                                                                
IN461    OC    TRPYCD,TRPYCD       CASH DISCOUNT                                
         BZ    TRANSEND                                                         
         ZAP   DUB,TRPYCD                                                       
         B     TRANSEND                                                         
         SPACE 1                                                                
IN462    MVC   0(20,R2),TRPYCLI    CLIENT                                       
         B     XIT                                                              
         SPACE 1                                                                
IN463    MVC   0(20,R2),TRPYPROD   PRODUCT                                      
         B     XIT                                                              
         SPACE 1                                                                
IN464    MVC   0(14,R2),TRPYINV    INVOICE NUMBER                               
         B     XIT                                                              
         SPACE 1                                                                
IN465    MVC   0(17,R2),TRPYPER    PERIOD                                       
         B     XIT                                                              
         SPACE 1                                                                
IN466    MVC   0(1,R2),TRPYTYPE    TYPE                                         
         B     XIT                                                              
         SPACE 1                                                                
IN467    EDIT  (2,TRPYEST),(3,0(R2)) ESTIMATE NUMBER                            
         B     XIT                                                              
         SPACE 1                                                                
IN468    ZAP   DUB,ACIOCR          CLEARED                                      
         B     TRANSEND                                                         
         SPACE 1                                                                
IN469    L     R4,ADTRANS          PAID                                         
         USING TRANSD,R4                                                        
         CLI   TRNSTYPE,X'81'                                                   
         BNE   TRANSEND                                                         
         ZAP   DUB,ACIODR                                                       
         B     TRANSEND                                                         
         SPACE 1                                                                
IN4610   L     R4,ADTRNREC         UNPAID                                       
         USING ACKEYD,R4                                                        
         OC    ACDTUSED,ACDTUSED                                                
         BNZ   TRANSEND                                                         
         DROP  R4                                                               
         ZAP   DUB,ACIOCR                                                       
         B     TRANSEND                                                         
         SPACE 1                                                                
OUT46    OC    0(6,R2),0(R2)       ANY DATE HERE                                
         BZ    XIT                 NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,0(R2)),(9,0(R3))   SHOW MMM/YY                    
         CLC   4(2,R2),=C'01'                                                   
         BL    XIT                                                              
         GOTO1 DATCON,DMCB,(0,0(R2)),(8,0(R3))     OR MMMDD/YY                  
         CLC   6(2,R2),=C'01'                                                   
         BL    XIT                                                              
         MVI   8(R3),C'-'                         AND OPTIONALLY                
         GOTO1 DATCON,DMCB,(0,6(R2)),(8,9(R3))       -MMMDD/YY                  
         B     XIT                                                              
         EJECT                                                                  
*              UK PAYABLES                                                      
         SPACE 3                                                                
*              ARGUMENT 1           ARGUMENT 1                                  
PAYINTAB DC    A(PAYIN1)            1=SERIAL NUMBER                             
         DC    A(PAYIN2)            2=INSERTION/TRANSMISSION DATE               
         DC    A(PAYIN3)            3=ACTIVITY DATE                             
         DC    A(PAYIN4)            4=CLIENT CODE                               
         DC    A(PAYIN5)            5=SHORT CLIENT NAME                         
         DC    A(PAYIN6)            6=SHORT PRODUCT NAME                        
         DC    A(PAYIN7)            7=SPACE/REMARKS                             
         DC    A(PAYIN8)            8=POSITION CODE                             
         DC    A(PAYIN9)            9=COLOUR CODE                               
         DC    A(PAYIN10)          10=SECONDS LENGTH                            
         DC    A(PAYIN11)          11=TIME                                      
         DC    A(PAYIN12)          12=GROSS AMOUNT                              
         DC    A(PAYIN13)          13=NET AMOUNT                                
         DC    A(PAYIN14)          14=VAT PAID                                  
         DC    A(PAYIN15)          15=SLUSH                                     
         SPACE 1                                                                
PAYIN    BAS   RE,SUBCHECK                                                      
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,TRPKELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TRPUKD,R4                                                        
         CLI   GLARGS,0                                                         
         BE    XIT                                                              
         CLI   GLARGS,15                                                        
         BH    XIT                                                              
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,PAYINTAB(R1)                                                  
         L     RF,0(R1)                                                         
         BR    RF                                                               
         SPACE 1                                                                
PAYIN1   MVC   0(1,R2),TRPKSER     SERIAL NUMBER                                
         XC    DUB,DUB                                                          
         MVC   DUB+1(3),TRPKSER+1                                               
         EDIT  (4,DUB),(7,1(R2)),FILL=0                                         
         B     XIT                                                              
         SPACE 1                                                                
PAYIN2   MVC   0(3,R2),TRPKIDAT    INSERTION/TRANSMISSION DATE                  
         B     XIT                                                              
         SPACE 1                                                                
PAYIN3   MVC   0(3,R2),TRPKADAT    ACTIVITY DATE                                
         B     XIT                                                              
         SPACE 1                                                                
PAYIN4   MVC   0(5,R2),TRPKCLIC    CLIENT CODE                                  
         B     XIT                                                              
         SPACE 1                                                                
PAYIN5   MVC   0(12,R2),TRPKCLI    CLIENT SHORT NAME                            
         B     XIT                                                              
         SPACE 1                                                                
PAYIN6   MVC   0(12,R2),TRPKPROD   PRODUCT SHORT NAME                           
         B     XIT                                                              
         SPACE 1                                                                
PAYIN7   MVC   0(15,R2),TRPKREM    SPACE/REMARKS                                
         B     XIT                                                              
         SPACE 1                                                                
PAYIN8   MVC   0(3,R2),TRPKPOS     POSITION CODE                                
         B     XIT                                                              
         SPACE 1                                                                
PAYIN9   MVC   0(3,R2),TRPKCOL     COLOUR CODE                                  
         B     XIT                                                              
         SPACE 1                                                                
PAYIN10  EDIT  (2,TRPKSEC),(3,0(R2))      SECONDS LENGTH                        
         B     XIT                                                              
         SPACE 1                                                                
PAYIN11  MVC   0(4,R2),TRPKTIM     TIME                                         
         B     XIT                                                              
         SPACE 1                                                                
PAYIN12  ZAP   0(8,R2),TRPKGRSS    GROSS AMOUNT                                 
         B     XIT                                                              
         SPACE 1                                                                
PAYIN13  ZAP   0(8,R2),TRPKNETP    NET PAID                                     
         B     XIT                                                              
         SPACE 1                                                                
PAYIN14  ZAP   0(8,R2),TRPKVATP    VAT PAID                                     
         B     XIT                                                              
         SPACE 1                                                                
PAYIN15  ZAP   0(8,R2),TRPKSLSH    SLUSH                                        
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
PAYOUT   MVC   LABLAREA(6),=C'CLIENT'                                           
         MVC   CODEAREA(5),0(R2)                                                
         CLI   GLARGS,4                                                         
         BE    GENOUT                                                           
         MVC   CODEAREA(5),MYSPACES                                             
         MVC   NAMEAREA(12),0(R2)                                               
         CLI   GLARGS,5                                                         
         BE    GENOUT                                                           
         MVC   LABLAREA(6),=C'PRODUCT'                                          
         B     GENOUT                                                           
         EJECT                                                                  
*              PRODUCTION - X'48' ELEMENTS                                      
         SPACE 3                                                                
*              ARGUMENT 1          0=NET BILLED                                 
*                                  1=COMMISSION                                 
*                                  2=VAT                                        
*                                  3=GROSS                                      
         SPACE 1                                                                
IN48     BAS   RE,SUBCHECK                                                      
         BNE   TRANSEND                                                         
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'44'                                                     
         BAS   RE,GETEL                                                         
         USING TRANSD,R4                                                        
         CLC   TRNSANAL,=C'99'                                                  
         BNE   TRANSEND                                                         
         TM    TRNSSTAT,X'80'                                                   
         BO    TRANSEND            REJECT DEBITS                                
         SPACE 1                                                                
         CLI   GLARGS,0            DEAL WITH NET NOW                            
         BNE   *+14                                                             
         ZAP   DUB,TRNSAMNT                                                     
         B     TRANSEND                                                         
         SPACE 1                                                                
         CLI   GLARGS,3            ADD NET BILL FOR GROSS                       
         BNE   *+10                                                             
         ZAP   DUB,TRNSAMNT                                                     
         SPACE 1                                                                
         LA    R4,TRNSNARR+15-2    THE DATA IS REALLY IN NARRATIVE              
         USING TRBILLD,R4                                                       
         CLI   GLARGS,2                                                         
         BE    *+10                                                             
         AP    DUB,TRBLCOMM        ADD COMMISSION FOR COMM & GROSS              
*&&UK                                                                           
         CLI   GLARGS,1                                                         
         BE    *+10                                                             
         AP    DUB,TRBLVAT1        ADD VAT FOR VAT & GROSS                      
*&&                                                                             
         B     TRANSEND                                                         
         EJECT                                                                  
*              PRODUCTION - X'4B' ELEMENTS                                      
         SPACE 3                                                                
*              ARGUMENT 1          0=CHARGES                                    
*                                  1=ALLOCATED                                  
*                                  2=UNALLOCATED                                
*                                  3=BILLED                                     
*                                  4=UNBILLED                                   
*              ARGUMENT 2          0=DOLLARS                                    
*                                  1=HOURS                                      
*              ARGUMENT 3          0=ALL BATCH TYPES                            
*                                  1=ONLY 1R CONTRAS                            
*                                  2=NO   1R CONTRAS                            
*                                  3=BATCH TYPE 57 ONLY                         
         SPACE 1                                                                
IN4B     CLI   ACMODE,PROCTRNS                                                  
         BNE   TRANSEND                                                         
         CLI   ACIOSBTY,0                                                       
         BNE   TRANSEND                                                         
         CLI   ACSUBMOD,PROC4B                                                  
         BE    TRANSEND                                                         
         SPACE 1                                                                
         OC    GLARGS+12(4),GLARGS+12    MOS FILTERING                          
         BZ    IN4BA                                                            
         CLC   ACIOMON,GLARGS+12   MONTH OF SERVICE ARGUMENTS                   
         BL    TRANSEND                                                         
         CLC   ACIOMON,GLARGS+14                                                
         BH    TRANSEND                                                         
         SPACE 1                                                                
*                                  NOTE - NO DATE CHECKING YET                  
IN4BA    L     R4,ADTRNREC                                                      
         USING ACKEYD,R4                                                        
         MVI   BILLSW,C'N'                                                      
         OC    ACDTUSED,ACDTUSED   TRANSACTION IS BILLED                        
         BZ    IN4BB               IF USED DATE IS NOT AFTER END                
*                                                                               
         BAS   RE,ISBURSON         IS THIS BURSON                               
         BE    IN4BB               YES, NEED TO LOOK AT 4B'S                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,ACDTUSED),(1,WORK+10)                             
         CLC   WORK+10(3),QTRAEND  IGNORE ITEMS BILLED AFTER                    
         BH    IN4BB                                                            
         MVI   BILLSW,C'Y'                                                      
         SPACE 1                                                                
IN4BB    MVI   ELCODE,X'44'                                                     
         BAS   RE,GETEL                                                         
         USING TRANSD,R4                                                        
         TM    TRNSSTAT,X'80'                                                   
         BNO   TRANSEND            ONLY WANT DEBITS                             
         SPACE 1                                                                
         CLI   GLARGS+2,3          ARG 3 CHECKING                               
         BE    IN4B493                                                          
         CLI   GLARGS+2,1          CHECKS FOR TYPE 1R CONTRAS                   
         BL    IN4B49X             ARG3=0 - ALL ALLOWED                         
         BH    IN4B492                                                          
         CLC   ACIOCON+1(2),=C'1R' ARG3=1 - 1R CONTRAS ONLY                     
         BNE   TRANSEND                                                         
         B     IN4B49X                                                          
         SPACE 1                                                                
IN4B492  CLC   ACIOCON+1(2),=C'1R' ARG3=2 - EXCLUDE 1R CONTRAS                  
         BE    TRANSEND                                                         
         B     IN4B49X                                                          
         SPACE 1                                                                
IN4B493  CLC   ACIOBT(2),=C'57'    ARG3=2 - BATCH TYPE 57 ONLY                  
         BNE   TRANSEND                                                         
         B     IN4B49X                                                          
         SPACE 1                                                                
IN4B49X  ZAP   DUB,TRNSAMNT        PICK UP DOLLARS                              
         CLI   GLARGS+1,1          BUT IF ARG2=1                                
         BNE   IN4B1                                                            
         ZAP   DUB,=P'0'                                                        
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'40'        WE WILL NEED HOURS                           
         BAS   RE,GETEL                                                         
         BNE   IN4B1                                                            
         USING ACPERSD,R4                                                       
         ZAP   DUB,ACPSHOUR                                                     
         DROP  R4                                                               
         SPACE 1                                                                
IN4B1    CLI   GLARGS,0            ARG1=0 ALL DONE                              
         BE    TRANSEND                                                         
         ZAP   SAVAMNT,DUB         (SAVE THIS AMOUNT)                           
         CLI   GLARGS,1            IF WE ARE LOOKING FOR ALLOCATED              
         BNE   *+10                                                             
         ZAP   DUB,=P'0'           WE HAVEN'T STARTED YET!                      
         CLI   BILLSW,C'Y'         IF TRANSACTION IS FULLY BILLED               
         BNE   IN4B1B                                                           
         CLI   GLARGS,3            IF WE ARE LOOKING FOR BILLED                 
         BE    TRANSEND               ALL DONE                                  
         CLI   GLARGS,4            IF WE ARE LOOKING FOR UNBILLED               
         BNE   IN4B1B                                                           
         ZAP   DUB,=P'0'              THE AMOUNT IS ZERO                        
         B     TRANSEND               AND WE'RE DONE                            
         SPACE 1                                                                
IN4B1B   CLI   GLARGS,3            IF WE ARE LOOKING FOR BILLED                 
         BNE   *+10                                                             
         ZAP   DUB,=P'0'           WE HAVEN'T STARTED YET!                      
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'4B'                                                     
         BAS   RE,GETEL                                                         
         B     IN4B4                                                            
         SPACE 1                                                                
IN4B2    BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
IN4B4    BNE   TRANSEND                                                         
         USING TRBDETD,R4                                                       
         MVC   WORK(4),TRBDAMNT    GET DOLLARS                                  
*&&US                                                                           
         CLI   GLARGS+1,1              OR, IF ARG2=1                            
         BNE   *+10                                                             
         MVC   WORK(4),TRBDHRS         HOURS                                    
*&&                                                                             
         L     R1,WORK                                                          
         CVD   R1,WORK                                                          
         SPACE 1                                                                
         CLI   GLARGS,2            TEST ARGUMENT FOR (UN)ALLOCATED              
         BL    IN4BALL                                                          
         BE    IN4BUNAL                                                         
         SPACE 1                                                                
         CLI   TRBDDTE,0           CHECK BILL DATE                              
         BE    IN4B2                                                            
         GOTO1 DATCON,DMCB,(2,TRBDDTE),(1,WORK+10)                              
         CLC   WORK+10(3),QTRAEND  IGNORE ITEMS BILLED AFTER                    
         BH    IN4B2                      REQUEST END DATE                      
         SPACE 1                                                                
         CLI   GLARGS,4            TEST ARGUMENT FOR (UN)BILLED                 
         BL    IN4BBILL                                                         
         BE    IN4BUNBL                                                         
         SPACE 1                                                                
*&&US                                                                           
*                                  ****** US DEFINITIONS *******                
IN4BALL  CLI   TRBDNO,X'40'        DEFINITION OF ALLOCATED IS IF                
         BH    IN4B2               BILL NUMBER IS SPACES                        
         AP    DUB,WORK(8)                                                      
         B     IN4B2                                                            
         SPACE 1                                                                
IN4BUNAL SP    DUB,WORK(8)         UNALLOCATED THE REST                         
         B     IN4B2                                                            
*&&                                                                             
         SPACE 1                                                                
*&&UK                                                                           
*                                  ****** UK DEFINITIONS *******                
IN4BALL  DS    0H                  DEFINITION OF ALLOCATED IS IF                
         CLI   TRBDNO,C' '            BILL NUMBER NOT = SPACES                  
         BE    IN4B2                                                            
         CLI   TRBDNO,C'#'            BILL NUMBER NOT START #                   
         BE    IN4B2                                                            
         AP    DUB,WORK(8)         USE TRBDAMNT                                 
         CP    WORK(8),=P'0'       UNLESS THIS IS = ZERO                        
         BNE   IN4B2                                                            
         ZAP   DUB,SAVAMNT         WHEN WE WILL USE TRNSAMNT                    
         B     TRANSEND            AND WE ARE DONE                              
         SPACE 1                                                                
IN4BUNAL DS    0H                  DEFINITION OF ALLOCATED IS IF                
         CLI   TRBDNO,C' '            BILL NUMBER NOT = SPACES                  
         BE    IN4B2                                                            
         CLI   TRBDNO,C'#'            BILL NUMBER NOT START #                   
         BE    IN4B2                                                            
         SP    DUB,WORK(8)         USE TRBDAMNT                                 
         CP    WORK(8),=P'0'       UNLESS THIS IS = ZERO                        
         BNE   IN4B2                                                            
         ZAP   DUB,=P'0'           WHEN WE WILL USE TRNSAMNT                    
         B     TRANSEND            AND WE ARE DONE                              
         SPACE 1                                                                
*&&                                                                             
         SPACE 1                                                                
SAVAMNT  DS    D                                                                
         SPACE 1                                                                
IN4BBILL CLI   TRBDNO,X'41'        DEFINITION OF BILLED IS IF                   
         BL    IN4B2               BILL NUMBER IS NOT SPACES                    
*                                                                               
         BAS   RE,ISBURSON         IS THIS BURSON                               
         BNE   *+12                NO                                           
         CLI   TRBDNO,C'W'         FOR BURSON, EXCLUDE WRITEOFFS FROM           
         BE    IN4B2               BILLED AMOUNT                                
*                                                                               
         AP    DUB,WORK(8)                                                      
         B     IN4B2                                                            
         SPACE 1                                                                
IN4BUNBL CLI   TRBDNO,X'41'        DEFINITION OF BILLED                         
         BL    IN4B2               BILL NUMBER IS NOT SPACES                    
         SP    DUB,WORK(8)                                                      
         B     IN4B2                                                            
*                                                                               
ISBURSON DS    0H                                                               
         CLC   ACIOAGYA,=C'YY'     IS THIS BURSON                               
         BE    ISBYES                                                           
         CLC   ACIOAGYA,=C'YF'                                                  
         BE    ISBYES                                                           
         LTR   RE,RE               RETURN NOT EQUAL                             
         BR    RE                                                               
ISBYES   CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*              PRODUCTION - X'4B' ELEMENTS EXCLUSIVE                            
         SPACE 3                                                                
*              ARGUMENT 1          1=BILL NUMBER                                
*                                  2=BILL DATE                                  
*                                  3=GROSS                                      
*                                  4=COMMISSION                                 
*                                  5=NET                                        
         SPACE 1                                                                
EX4B     CLI   ACSUBMOD,PROC4B     ONLY INTERESTED IN 4B ELEMENTS               
         BNE   TRANSEND                                                         
         L     R4,ADTRNREC                                                      
         USING ACKEYD,R4                                                        
         XC    USEDAMNT,USEDAMNT   FIGURE USED AMOUNT                           
         OC    ACDTUSED,ACDTUSED   TRANSACTION IS BILLED                        
         BZ    EX4B1                                                            
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         ZAP   DUB,TRNSAMNT        SO USE TRANS AMOUNT                          
         CVB   R1,DUB                                                           
         ST    R1,USEDAMNT                                                      
         SPACE 1                                                                
EX4B1    L     R4,ADCUREL                                                       
         USING TRBDETD,R4                                                       
         OC    TRBDDTE,TRBDDTE     THAT ARE ACTUALLY BILLED                     
         BZ    XIT                                                              
         SPACE 1                                                                
         MVC   SAVEDTE,ACIODTE                                                  
         CLI   QFILTDTY,QFILTDBI   BILL DATE FILTERING?                         
         BNE   EX4B2                                                            
         GOTO1 DATCON,DMCB,(2,TRBDDTE),(1,ACIODTE)                              
         SPACE 1                                                                
EX4B2    BAS   RE,DATEFILT                                                      
         MVC   ACIODTE,SAVEDTE                                                  
         BNE   XIT                                                              
         MVC   0(6,R2),TRBDNO                                                   
         CLI   GLARGS,2                                                         
         BL    XIT                                                              
         MVC   0(2,R2),TRBDDTE                                                  
         BE    XIT                                                              
         MVC   0(4,R2),TRBDAMNT    NET                                          
         OC    0(4,R2),0(R2)       IF ZERO,                                     
         BNZ   *+10                                                             
         MVC   0(4,R2),USEDAMNT    TRY USED AMOUNT                              
         CLI   GLARGS,5                                                         
         BE    EX4B4                                                            
         TM    TRBDCMST,TRBDCPAK   IS COMMISSION FIELD PACKED?                  
         BZ    EX4B3               NO, IT MUST BE BINARY                        
         ZAP   DUB,TRBDCMP         (COMM RATE 4 DECS)                           
         CVB   R0,DUB                                                           
         B     *+8                                                              
*                                                                               
EX4B3    ICM   R0,15,TRBDCMP                                                    
         L     R1,0(R2)                                                         
         MR    R0,R0                                                            
         D     R0,=F'500000'                                                    
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                (COMM IN R1)                                 
         L     R0,0(R2)            (NET)                                        
         ST    R1,0(R2)                                                         
         CLI   GLARGS,4                                                         
         BE    EX4B4                                                            
         AR    R1,R0                                                            
         ST    R1,0(R2)                                                         
         SPACE 1                                                                
EX4B4    L     R1,0(R2)                                                         
         CVD   R1,DUB                                                           
         BAS   RE,ARGFILT                                                       
         ZAP   0(8,R2),DUB                                                      
         B     XIT                                                              
         SPACE 1                                                                
SAVEDTE  DS    XL3                                                              
USEDAMNT DS    F                                                                
         EJECT                                                                  
*              TALENT ETC - X'4F' ELEMENTS                                      
         SPACE 3                                                                
*              ARGUMENT 1          0=AGENCY                                     
*                                  1=CLIENT                                     
*                                  2=PRODUCT                                    
*                                  3=COMMERCIAL                                 
*                                  4=EMPLOYER                                   
*                                  5=CATEGORY                                   
         SPACE 1                                                                
IN4F     CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'4F'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TRCPJD,R4                                                        
         GOTO1 HEXOUT,DMCB,TRCPUL+1,(R2),1,=C'TOG'                              
         CLI   GLARGS,1            AGENCY=0                                     
         BL    XIT                                                              
         MVC   0(3,R2),TRCPCLI     CLIENT=1                                     
         BE    XIT                                                              
         MVC   0(3,R2),TRCPPROD    PRODUCT=2                                    
         CLI   GLARGS,3                                                         
         BL    XIT                                                              
         MVC   0(6,R2),TRCPCOM     COMMERCIAL=3                                 
         BE    XIT                                                              
         MVC   0(3,R2),TRCPEMP     EMPLOYER=4                                   
         CLI   GLARGS,5                                                         
         BL    XIT                                                              
         MVC   0(3,R2),TRCPCAT     CAT=5                                        
         B     XIT                                                              
         EJECT                                                                  
*              MONTHLY SALARY - X'52' ELEMENTS                                  
         SPACE 3                                                                
*              ARGUMENT 1          1=SALARY FOR PERIOD                          
*                                  2=OVERTIME FOR PERIOD                        
*                                  3=TEMPORARY FOR PERIOD                       
*                                  4=BONUS                                      
*                                  5=BENEFIT                                    
*                                  6=ADMINISTRATION                             
*                                  7=BUDGETED SALARY                            
*                                  8=HOURLY RATE                                
*                                  9=PENSION                                    
         SPACE 1                                                                
IN52     ZAP   0(8,R2),=P'0'       CLEAR AMOUNT                                 
         CLI   ACMODE,PROCACC      ONLY HANDLE AT PROCACC TIME                  
         BNE   XIT                                                              
         CLC   ACIOACC+1(2),=C'1R' ONLY 1R ACCOUNTS                             
         BNE   XIT                                                              
         CLI   GLARGS,1            VALIDATE ARGUMENTS HERE                      
         BL    XIT                                                              
         CLI   GLARGS,9                                                         
         BH    XIT                                                              
         LA    R3,PPERIOD                                                       
         OC    GLARGS+12(2),GLARGS+12                                           
         BZ    *+8                                                              
         LA    R3,GLARGS+12                                                     
         LA    R4,BLOCK                                                         
         L     RF,=V(ACSLRY)                                                    
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,ADACCREC,(R3),(R4)                                     
*                                                                               
         USING SLRD,R4                                                          
         LA    R4,BLOCK                                                         
         ZIC   R1,GLARGS                                                        
         LA    R4,SLRSAL           ADDRESS FIRST BUCKET                         
*                                                                               
IN52A    ZAP   DUB,0(6,R4)         SAVE IN CASE THIS IS IT                      
         LA    R4,7(0,R4)          BUMP TO NEXT BUCKET                          
         BCT   R1,IN52A                                                         
         ZAP   0(8,R2),DUB         SAVE FINAL BUCKET                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              EMPLOYEE HISTORY - X'56' ELEMENTS                                
         SPACE 3                                                                
*              ARGUMENT 1          1=HIRE DATE                                  
*                                  2=TERMINATION DATE                           
         SPACE 1                                                                
IN56     L     R4,ADACCREC                                                      
         MVI   ELCODE,X'56'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACEMPD,R4                                                        
         MVC   0(3,R2),ACEMPHIR    HIRE DATE=1                                  
         CLI   GLARGS,2                                                         
         BL    XIT                                                              
         MVC   0(3,R2),ACEMPTRM    TERM DATE=2                                  
         B     XIT                                                              
         EJECT                                                                  
*              SALES/USE TAX RATES - X'5F' ELEMENTS                             
         SPACE 3                                                                
*              ARGUMENT 1          1=EFFECTIVE DATE                             
*                                  2=TAX RATE                                   
*                                  3=TAX BASIS                                  
*                                  4=LOCALITY L1                                
*                                  5=LOCALITY L2                                
*                                  6=LOCALITY L3                                
*                                  7=LOCALITY L4                                
         SPACE 1                                                                
IN5F     CLI   ACMODE,PROCTRNS                                                  
         BNE   XIT                                                              
         L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'5F'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACTAXD,R4                                                        
         MVC   0(3,R2),ACTAXEFF    EFFECTIVE DATE=1                             
         CLI   GLARGS,2                                                         
         BL    XIT                                                              
         MVC   0(4,R2),ACTAXRTE    TAX RATE=2                                   
         BE    XIT                                                              
         CLI   GLARGS,3                                                         
         BNE   IN5F2                                                            
         MVC   0(6,R2),ACTAXBAS    TAX BASIS=3                                  
         BAS   RE,DATEFILT         CAN FILTER ON DATE                           
         BE    XIT                                                              
         ZAP   0(6,R2),=P'0'       FAILED - SO RETURN ZERO                      
         B     XIT                                                              
         SPACE 1                                                                
IN5F2    MVC   0(15,R2),MYSPACES   LOCALITIES 1-4=4-7                           
         USING ACUTKEY,R2                                                       
         MVI   ACUTTYPE,ACUTEQU                                                 
         MVI   ACUTSREC,ACUTSEQU                                                
         MVC   ACUTCMP,ACIOACC                                                  
         ZIC   R1,GLARGS           (4-7)                                        
         SH    R1,=H'3'            (1-4)                                        
         SLA   R1,1                (2-8)                                        
         BCTR  R1,0                (1-7 FOR EX)                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACUTLOC(0),ACTAXLOC+3                                            
         CLI   GLARGS+1,C'N'                                                    
         BNE   XIT                                                              
         GOTO1 GETNAME,DMCB,0(R2),BLOCK                                         
         MVC   0(36,R2),BLOCK                                                   
         B     XIT                                                              
         SPACE 1                                                                
OUT5F    MVC   LABLAREA(8),=C'LOCALITY'                                         
         CLI   GLARGS+1,C'N'                                                    
         BNE   OUT5F2                                                           
         MVC   NAMEAREA(36),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
OUT5F2   ZIC   R1,GLARGS           PICK OUT PIECE OF CODE                       
         SH    R1,=H'4'                                                         
         SLA   R1,1                                                             
         USING ACUTKEY,R2                                                       
         LA    R1,ACUTLOC(R1)                                                   
         MVC   CODEAREA(2),0(R1)                                                
         CLI   GLARGS+1,C'C'                                                    
         BE    GENOUT                                                           
         GOTO1 GETNAME,DMCB,0(R2),BLOCK                                         
         MVC   NAMEAREA(36),BLOCK                                               
         B     GENOUT                                                           
         EJECT                                                                  
*              EXTRA STATUS ELEMENT - X'60'                                     
         SPACE 3                                                                
*              ARGUMENT 1          1=TIME TYPE                                  
         SPACE 1                                                                
IN60     L     R4,ADTRNREC                                                      
         MVI   ELCODE,X'60'                                                     
         MVI   0(R2),C'N'          TYPE OF TIME SHEET - N=NONE                  
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TRSELD,R4                                                        
         CLI   GLARGS,1                                                         
         BE    IN601                                                            
         B     XIT                                                              
         SPACE 1                                                                
IN601    MVI   0(R2),C'R'          TYPE OF TIME SHEET - R=REGULAR               
         TM    TRSSTAT2,X'08'                                                   
         BNO   *+8                                                              
         MVI   0(R2),C'A'          A=ADJUSTED                                   
         TM    TRSSTAT2,X'04'                                                   
         BNO   *+8                                                              
         MVI   0(R2),C'M'          M=MISSING                                    
         TM    TRSSTAT2,X'01'                                                   
         BNO   *+8                                                              
         MVI   0(R2),C'T'          T=TIME                                       
         B     XIT                                                              
         EJECT                                                                  
*              BUDGET HANDLING                                                  
         SPACE 3                                                                
*              ARGUMENTS           1-2 BUDGET NUMBER (BINARY)                   
*                                  7-12  ARGUMENTS TO ARGFILT                   
*                                  13-16 START/END YEAR/MONTH PWOS              
         SPACE 1                                                                
BUDGET   ZAP   DUB,=P'0'           BUDGET                                       
         CLI   ACMODE,PROCBUDG                                                  
         BNE   BUDGEND                                                          
         CLC   GLARGS(2),ACIOBCOD  CHECK BUDGET CODE MATCHES                    
         BNE   BUDGEND                                                          
         LA    R1,PPERIOD          DEFAULT IS REQUEST PERIOD                    
         OC    GLARGS+12(4),GLARGS+12                                           
         BZ    *+8                                                              
         LA    R1,GLARGS+12        ELSE PICK UP COLUMN S/E                      
         CLC   ACIOMON,0(R1)       CHECK DATE NOT BEFORE START                  
         BL    BUDGEND                                                          
         CLC   ACIOMON,2(R1)       CHECK DATE NOT AFTER END                     
         BH    BUDGEND                                                          
         ZAP   DUB,ACIOAMNT                                                     
         SPACE 1                                                                
BUDGEND  BAS   RE,ARGFILT          MAY BE ARGUMENT FILTERS                      
         ZAP   0(8,R2),DUB                                                      
         CLI   BUDGACSW,0          SET THAT WE HAVE BEEN HERE ONCE              
         BNE   *+8                                                              
         MVI   BUDGACSW,1                                                       
         CP    DUB,=P'0'           AND SET TO 2 IF ANY SIGNIFICANCE             
         BE    XIT                                                              
         MVI   BUDGACSW,2                                                       
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              BUDGET HEADLINES                                                 
         SPACE 3                                                                
*              ARGUMENTS           1-2 BUDGET NUMBER (BINARY)                   
         SPACE 1                                                                
BUDGHEAD B     XIT                                                              
         EJECT                                                                  
*              PERIOD IN HEADINGS                                               
         SPACE 3                                                                
*              ARGUMENTS           1-4 START/END YEAR/MONTH PWOS                
*                                      START MAY BE ZERO                        
         SPACE 1                                                                
PERPOP   MVC   WORK,MYSPACES        PERIOD TO HEADLINES                         
         L     R4,GLADTENT                                                      
         USING DRHDD,R4                                                         
         CLI   DRHDWDTH,6           IS THERE ROOM FOR MONTH/YEAR?               
         BL    XIT                                                              
         CLI   GLARGS+4,X'FE'                                                   
         BE    PPDATE                                                           
         CLI   GLARGS+4,X'FF'                                                   
         BE    PPAGE                                                            
         MVC   WORK(3),=C'END'                                                  
         CLI   GLARGS,0            IF START OMITTED SHOW END MMMYY              
         BE    PERPOP1                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(2),GLARGS                                                    
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(6,WORK)                                     
         MVC   WORK+3(3),WORK+4    (GET RID OF /)                               
         CLC   GLARGS(2),GLARGS+2                                               
         BE    PERPOP2                                                          
         MVI   WORK+5,C'-'                                                      
         SPACE 1                                                                
PERPOP1  XC    DUB,DUB                                                          
         MVC   DUB(2),GLARGS+2                                                  
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(6,WORK+6)                                   
         MVC   WORK+9(3),WORK+10                                                
         CLI   DRHDWDTH,10          IS THERE ROOM FOR MMMYY-MMMYY               
         BH    PERPOP2                                                          
         MVC   WORK+3(9),WORK+5                                                 
         CLI   DRHDWDTH,8           TRY MMM-MMMYY                               
         BH    PERPOP2                                                          
         MVC   WORK+7(2),MYSPACES                                               
         CLI   DRHDWDTH,6           NOW DOWN TO MMM-MMM                         
         BH    PERPOP2                                                          
         B     XIT                                                              
         SPACE 1                                                                
PPDATE   GOTO1 DATCON,DMCB,(X'12',GLARGS),(16,WORK)                             
         CLC   GLARGS(2),GLARGS+2                                               
         BNE   PERPOP2                                                          
         MVC   WORK+5(6),MYSPACES  START=END - CLEAN UP                         
         B     PERPOP2                                                          
         SPACE 1                                                                
PPAGE    MVC   WORK(11),GLARGS+5                                                
         MVC   WORK+11(4),=C'DAYS'                                              
         SPACE 1                                                                
PERPOP2  GOTO1 SQUASHER,DMCB,WORK,20                                            
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+7(1),DRHDWDTH                                               
         GOTO1 CENTER,DMCB,WORK                                                 
         ZIC   R1,DRHDWDTH                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),WORK                                                     
         SPACE 1                                                                
COMPUTIN ZAP   0(8,R2),=P'0'                                                    
         B     XIT                                                              
         SPACE 1                                                                
COUNTIN  CLI   ACMODE,PROCACC      COUNT N'ACCOUNTS                             
         BNE   XIT                                                              
         MVI   3(R2),1             (4 BYTE BINARY)                              
         B     XIT                                                              
         SPACE 1                                                                
GAPIN    MVC   0(4,R2),MYSPACES                                                 
         B     XIT                                                              
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
MYSPACES DC    CL198' '                                                         
MYMONTHS DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
BALDATES DS    CL4                                                              
DONTLEV  DC    X'00'                                                            
FILTBYTE DS    CL1                                                              
FIRST86  DS    CL1                                                              
PROF86   DS    CL16                                                             
BILLSW   DS    CL1                                                              
RELO     DS    A                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACGENINPT                                                      
         EJECT                                                                  
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
         DC    C'ACCHEAD ',A(ACCHEAD)                                           
         DC    C'ACCIN   ',A(ACCIN)                                             
         DC    C'ACCOUT  ',A(ACCOUT)                                            
         DC    C'ACCTOTAL',A(ACCTOTAL)                                          
*&&US*&& DC    C'AGYIN   ',A(AGYIN)                                             
*&&US*&& DC    C'AGYOUT  ',A(AGYOUT)                                            
         DC    C'ATTIN   ',A(ATTIN)                                             
         DC    C'ATTOUT  ',A(ATTOUT)                                            
         DC    C'BRIN    ',A(BRIN)                                              
         DC    C'BROUT   ',A(BROUT)                                             
         DC    C'BTIN    ',A(BTIN)                                              
         DC    C'BTOUT   ',A(BTOUT)                                             
         DC    C'BUCKET  ',A(BUCKET)                                            
         DC    C'BUDGET  ',A(BUDGET)                                            
         DC    C'BUDGHEAD',A(BUDGHEAD)                                          
*&&US*&& DC    C'CATIN   ',A(CATIN)                                             
         DC    C'CHECK   ',A(CHECK)                                             
         DC    C'CKDTOUT ',A(CKDTOUT)                                           
         DC    C'CLIOUT  ',A(CLIOUT)                                            
         DC    C'COMPIN  ',A(COMPIN)                                            
         DC    C'COMPOUT ',A(COMPOUT)                                           
         DC    C'COMPUTIN',A(COMPUTIN)                                          
         DC    C'COSTIN  ',A(COSTIN)                                            
         DC    C'COSTOUT ',A(COSTOUT)                                           
         DC    C'COUNTIN ',A(COUNTIN)                                           
         DC    C'DATEIN  ',A(DATEIN)                                            
         DC    C'DATOUT  ',A(DATOUT)                                            
         DC    C'DTE2OUT ',A(DTE2OUT)                                           
         DC    C'FILTIN  ',A(FILTIN)                                            
         DC    C'FILTOUT ',A(FILTOUT)                                           
         DC    C'GAPIN   ',A(GAPIN)                                             
         DC    C'GSTIN   ',A(GSTIN)                                             
         DC    C'IN1A    ',A(IN1A)                                              
         DC    C'IN32    ',A(IN32)                                              
         DC    C'IN40    ',A(IN40)                                              
         DC    C'IN46    ',A(IN46)                                              
         DC    C'IN48    ',A(IN48)                                              
         DC    C'IN4B    ',A(IN4B)                                              
         DC    C'IN4F    ',A(IN4F)                                              
         DC    C'IN52    ',A(IN52)                                              
         DC    C'IN56    ',A(IN56)                                              
         DC    C'IN5F    ',A(IN5F)                                              
         DC    C'IN60    ',A(IN60)                                              
         DC    C'EX4B    ',A(EX4B)                                              
         DC    C'LEDGIN  ',A(LEDGIN)                                            
         DC    C'LEDGOUT ',A(LEDGOUT)                                           
         DC    C'MGIN    ',A(MGIN)                                              
         DC    C'MGOUT   ',A(MGOUT)                                             
         DC    C'MEDIN   ',A(MEDIN)                                             
         DC    C'MEDOUT  ',A(MEDOUT)                                            
         DC    C'MONIN   ',A(MONIN)                                             
         DC    C'MONOUT  ',A(MONOUT)                                            
         DC    C'NARRIN  ',A(NARRIN)                                            
         DC    C'NUMIN   ',A(NUMIN)                                             
         DC    C'OFFIN   ',A(OFFIN)                                             
         DC    C'OFFOUT  ',A(OFFOUT)                                            
         DC    C'OGIN    ',A(OGIN)                                              
         DC    C'OGOUT   ',A(OGOUT)                                             
         DC    C'ORDIN   ',A(ORDIN)                                             
         DC    C'OTHERIN ',A(OTHERIN)                                           
         DC    C'OUT46   ',A(OUT46)                                             
         DC    C'OUT5F   ',A(OUT5F)                                             
         DC    C'PAYIN   ',A(PAYIN)                                             
         DC    C'PAYOUT  ',A(PAYOUT)                                            
         DC    C'PERPOP  ',A(PERPOP)                                            
*&&US*&& DC    C'PERFOUT ',A(PERFOUT)                                           
         DC    C'REFIN   ',A(REFIN)                                             
         DC    C'REFOUT  ',A(REFOUT)                                            
         DC    C'REPIN   ',A(REPIN)                                             
         DC    C'PINTIN  ',A(PINTIN)                                            
         DC    C'PUBIN   ',A(PUBIN)                                             
         DC    C'QUARTIN ',A(QUARTIN)                                           
         DC    C'QUARTOUT',A(QUARTOUT)                                          
         DC    C'SALTIN  ',A(SALTIN)                                            
         DC    C'SALTOUT ',A(SALTOUT)                                           
         DC    C'SUBREF  ',A(SUBREF)                                            
         DC    C'STATIN  ',A(STATIN)                                            
         DC    C'TALIN   ',A(TALIN)                                             
         DC    C'TEIN    ',A(TEIN)                                              
         DC    C'TEOUT   ',A(TEOUT)                                             
         DC    C'TRANS   ',A(TRANS)                                             
         DC    C'UNIQUE  ',A(UNIQUE)                                            
         DC    C'UNITIN  ',A(UNITIN)                                            
         DC    C'UNITOUT ',A(UNITOUT)                                           
         DC    C'USEDOUT ',A(USEDOUT)                                           
         DC    C'USECOUT ',A(USECOUT)                                           
         DC    C'VENDIN  ',A(VENDIN)                                            
         DC    C'VENDOUT ',A(VENDOUT)                                           
         DC    C'WEEKIN  ',A(WEEKIN)                                            
         DC    C'WEEKOUT ',A(WEEKOUT)                                           
         DC    C'WGIN    ',A(WGIN)                                              
         DC    C'WGOUT   ',A(WGOUT)                                             
         DC    C'WORKIN  ',A(WORKIN)                                            
         DC    C'WORKOUT ',A(WORKOUT)                                           
         DC    C'YEARIN  ',A(YEARIN)                                            
         DC    C'YEAROUT ',A(YEAROUT)                                           
         DC    X'FF'                                                            
* DRGLOBAL                                                                      
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         PRINT ON                                                               
* DRIVETABLE                                                                    
         PRINT OFF                                                              
       ++INCLUDE DRIVETABLE                                                     
         PRINT ON                                                               
* DRINTRECD2                                                                    
         PRINT OFF                                                              
       ++INCLUDE DRINTRECD2                                                     
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACWRIWORKD                                                     
       ++INCLUDE DDSLRD                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACSYSDRIVE05/01/03'                                      
         END                                                                    
