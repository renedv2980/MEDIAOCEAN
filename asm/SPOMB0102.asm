*          DATA SET SPOMB0102  AT LEVEL 026 AS OF 05/01/02                      
*PHASE SP0102,+0                                                                
         TITLE 'SPOMB0102 - OM STATION BUCKETS'                                 
SP0102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SP01**                                                       
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         A     R9,=F'4096'                                                      
         USING SPWORKD,RA,R9                                                    
         CLI   MODE,REQFRST                                                     
         BE    PROCESS                                                          
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
PROCESS  DS    0H                                                               
*                                                                               
OPEN     OPEN     (OMBIN,(INPUT))                                               
*                                                                               
         SR    R0,R0               SET BINSRCH PARS                             
         L     R1,=A(OMBTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,OMBTABL                                                       
         LA    R4,OMBTABKL                                                      
         LA    R5,OMBTABMX                                                      
         STM   R0,R5,BSPARS                                                     
*                                                                               
GETOMB   LA    R0,OMBREC                                                        
         LA    R1,OMBIN                                                         
         GET   (1),(0)                                                          
*                                                                               
         LA    R3,OMBTABW                                                       
         USING OMBTABD,R3                                                       
         CLC   QMED,EDMED          TEST MEDIA                                   
         BNE   GETOMB                                                           
         CLC   =C'ED',OMBREC     EST DETAIL                                     
         BE    GB20                                                             
         CLC   =C'IH',OMBREC      INV HEADER                                    
         BE    GB30                                                             
         B     GETOMB                                                           
*                                                                               
GB20     DS    0H                                                               
         MVC   OMED(10),EDMED                                                   
*                                  LOOK UP MKT                                  
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),EDMED                                                   
         MVC   KEY+2(5),EDSTA                                                   
         MVC   KEY+7(2),QAGY                                                    
         MVC   KEY+9(3),EDCLT                                                   
*                                                                               
         GOTO1 READSTA                                                          
         L     RF,ADSTAT                                                        
         MVC   OMKT,SMKT-STAREC(RF)                                             
*                                                                               
         PACK  DUB,EDNET                                                        
         CP    DUB,=P'0'                                                        
         BNE   GB22                                                             
         PACK  DUB,EDGROSS                                                      
         CVB   R1,DUB                                                           
         M     R0,=F'85000'                                                     
         L     RF,=F'100000'                                                    
         BAS   RE,DIV                                                           
         CVD   R1,DUB                                                           
         UNPK  EDNET,DUB                                                        
*                                                                               
GB22     DS    0H                                                               
         MVC   OSTA,EDSTA                                                       
         MVC   OINV(10),SPACES                                                  
         MVC   OSPT(24),EDSPOTS                                                 
         GOTO1 BINSRCH,BSPARS,(1,OMBTABD)                                       
         B     GETOMB                                                           
*                                                                               
DIV      DIV   (R0),(RF)                                                        
*                                                                               
GB30     DS    0H                                                               
         MVC   OMED(10),IHMED                                                   
         MVC   OMKT(9),SPACES                                                   
         MVC   OINV(4),IHINVNO+2                                                
         MVC   OIDT(6),IHINVDT                                                  
         MVC   OSPT,SPACES                                                      
         MVC   OGRS(20),IHGROSS                                                 
         GOTO1 BINSRCH,BSPARS,(1,OMBTABD)                                       
         B     GETOMB                                                           
*                                                                               
GBEOF    DS    0H                                                               
         CLOSE OMBIN                                                            
*                                                                               
         L     R7,ADBUY                                                         
         ST    R7,AREC                                                          
         USING STABUCK,R7                                                       
*                                                                               
         L     R3,=A(OMBTAB)       START OF TABLE                               
         USING OMBTABD,R3                                                       
         L     R6,BSPARS+8         COUNT                                        
*                                                                               
EF4      DS    0H                                                               
         CLI   OMKT,C' '           NO MKT MEANS IH                              
         BNE   EF8                                                              
         ZAP   DGRS,=P'0'                                                       
         ZAP   DNET,=P'0'                                                       
         MVC   HKEY(10),OMED                                                    
         MVC   HINV(10),OINV                                                    
         MVC   HGRS,OGRS                                                        
         MVC   HNET,ONET                                                        
         GOTO1 REPORT                                                           
         MVC   P(OMBTABL),OMED                                                  
         LA    R2,P+OMBTABL+4                                                   
         MVC   0(10,R2),HGRS                                                    
         MVC   14(10,R2),HNET                                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         B     EF60                                                             
*                                                                               
EF8      DS    0H                                                               
         CLC   OMED(10),HKEY       SAME AS IH                                   
         BNE   EF60                NO, SKIP                                     
*                                                                               
         XC    STABUCK(256),STABUCK                                             
*                                                                               
         PACK  DUB,OGRS                                                         
         CVB   R0,DUB                                                           
         STCM  R0,15,STABGRS                                                    
         AP    DGRS,DUB                                                         
         PACK  DUB,ONET                                                         
         CVB   R0,DUB                                                           
         STCM  R0,15,STABNET                                                    
         AP    DNET,DUB                                                         
         PACK  DUB,OSPT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,STABSPTS                                                    
*                                                                               
         PACK  DUB,HINV                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,STABINV                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,HIDT),(2,STABBDT)                                 
*                                                                               
         MVC   STABPER,=X'5701'                                                 
         MVC   STABELEM(2),=X'0E12'                                             
*                                                                               
         MVC   STABKCOD,=X'0E01'                                                
         MVC   STABKAM,BAGYMD                                                   
         GOTO1 CLPACK,DMCB,OCLT,STABKCLT                                        
*                                  LOOK UP PRD CODE                             
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),STABKCLT                                                
         MVC   KEY+4(3),OPRD                                                    
         GOTO1 READ                                                             
         GOTO1 GETPRD                                                           
         L     RF,ADPRD                                                         
         MVC   STABKPRD,PCODE+1-PRDHDR(RF)                                      
*                                                                               
         PACK  DUB,OEST                                                         
         CVB   R0,DUB                                                           
         STC   R0,STABKEST                                                      
         GOTO1 MSPACK,DMCB,OMKT,OSTA,STABKMKT                                   
*                                                                               
         MVC   STABLEN,=AL2(42)                                                 
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   EF40                                                             
         GOTO1 ADD                                                              
*                                                                               
EF40     DS    0H                                                               
         MVC   P(OMBTABL),OMED                                                  
         LA    R2,P+OMBTABL+2                                                   
         EDIT  (P6,DGRS),(12,00(R2)),ZERO=NOBLANK                               
         EDIT  (P6,DNET),(12,14(R2)),ZERO=NOBLANK                               
         GOTO1 REPORT                                                           
*                                                                               
         L     R5,AREC                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
         B     EF60                                                             
*                                                                               
EF60     DS    0H                                                               
         A     R3,BSPARS+12        NEXT ENTRY                                   
         BCT   R6,EF4                                                           
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
DMPREC   NTR1                                                                   
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         SR    R2,R2                                                            
         ICM   R2,3,13(R5)         LENGTH                                       
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         GOTO1 REPORT                                                           
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         SPACE 2                                                                
OMBIN    DCB   DDNAME=OMBIN,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=GBEOF                                                      
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
HKEY     DS    CL10                                                             
HINV     DS    CL4                                                              
HIDT     DS    CL6                                                              
HGRS     DS    CL10                                                             
HNET     DS    CL10                                                             
DGRS     DS    PL6                                                              
DNET     DS    PL6                                                              
*                                                                               
BSPARS   DS    6F                                                               
*                                                                               
         DS    0D                                                               
OMBTABW  DS    CL(OMBTABL)                                                      
*                                                                               
         DS    0D                                                               
OMBREC   DS    1000C                                                            
*                                                                               
         ORG   OMBREC                                                           
EDRECD   DS    0CL80               ** ESTIMATE DETAIL RECORD DSECT **           
*                                                                               
EDTYPE   DS    CL2'ED'  A          RECORD TYPE                                  
EDMED    DS    CL1       A         MEDIA CODE                                   
EDCLT    DS    CL3       A         DDS CLIENT CODE                              
EDPRD    DS    CL3       A         DDS PRODUCT CODE                             
EDEST    DS    CL3       N         ESTIMATE NUMBER                              
EDJWCLT  DS    CL3       N         JWT CLIENT CODE                              
EDJWPRD  DS    CL4       N         JWT PRODUCT CODE                             
*                                                                               
EDMONSVC DS    CL4       N         MONTH OF SERVICE (YYMM)                      
EDSTA    DS    CL5      A          STATION (+ A/F/T)                            
         DS    CL18                SPARE                                        
EDSPOTS  DS    CL4                 SPOTS                                        
EDGROSS  DS    CL10      N         GROSS DOLLARS (2 DEC)                        
EDNET    DS    CL10      N         NET DOLLARS                                  
EDCOMM   DS    CL10      N         COMMISSION                                   
         SPACE 2                                                                
         ORG   OMBREC                                                           
IHRECD   DS    0CL80               ** INVOICE HEADER RECORD DSECT **            
*                                                                               
IHTYPE   DS    CL2'IH'   A         RECORD TYPE                                  
IHMED    DS    CL1       A         MEDIA CODE                                   
IHCLT    DS    CL3       A         DDS CLIENT CODE                              
IHPRD    DS    CL3       A         DDS PRODUCT CODE                             
IHEST    DS    CL3       N         ESTIMATE NUMBER                              
IHJWCLT  DS    CL3       N         JWT CLIENT CODE                              
IHJWPRD  DS    CL4       N         JWT PRODUCT CODE                             
*                                                                               
IHMONSVC DS    CL4       N         MONTH OF SERVICE (YYMM)                      
IHINVNO  DS    CL6       N         INVOICE NUMBER                               
IHINVTYP DS    CL1       A         INVOICE TYPE (S=SUMM/D=DETAIL)               
IHINVDT  DS    CL6       N         INVOICE DATE (YYMMDD)                        
IHDUEDT  DS    CL6       N         DUE DATE (YYMMDD)                            
         DS    CL8                                                              
IHGROSS  DS    CL10      N         GROSS DOLLARS (2 DEC)                        
IHNET    DS    CL10      N         NET DOLLARS                                  
IHCOMM   DS    CL10      N         COMMISSION                                   
         ORG                                                                    
*                                                                               
         DS    0D                                                               
OMBTAB   DS    CL(OMBTABMX*OMBTABL)                                             
*                                                                               
         SPACE 3                                                                
OMBTABD  DSECT                                                                  
OMED     DS    CL1                                                              
OCLT     DS    CL3                                                              
OPRD     DS    CL3                                                              
OEST     DS    CL3                                                              
OMKT     DS    CL4                                                              
OSTA     DS    CL5                                                              
OINV     DS    CL4                                                              
OIDT     DS    CL6                                                              
OSPT     DS    CL4                                                              
OGRS     DS    CL10                                                             
ONET     DS    CL10                                                             
OMBTABL  EQU   *-OMBTABD                                                        
OMBTABKL EQU   *-OMBTABD                                                        
OMBTABMX EQU   500                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENSTAB                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPOMB0102 05/01/02'                                      
         END                                                                    
