*          DATA SET DDCCLIB2   AT LEVEL 165 AS OF 07/17/07                      
*PHASE CCLIB2A                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE XSORT                                                                  
*                                                                               
         TITLE 'EXTRACT PHASE CSECT LEVELS'                                     
         SPACE 1                                                                
CCLIB1   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**CCLI**,=V(REGSAVE),RA,R9                                     
*                                                                               
         LA    R1,CSECTQ                                                        
         BAS   RE,GETMAIN                                                       
         ST    R1,ACSECTAB                                                      
         LA    R1,BLDLQ                                                         
         BAS   RE,GETMAIN                                                       
         ST    R1,ABLDLIST                                                      
         LA    R1,IOAREAQ                                                       
         BAS   RE,GETMAIN                                                       
         ST    R1,APREVSV          30 BYTES FROM PREV REC                       
         LA    R1,30(R1)                                                        
         ST    R1,AIOAREA          THIS RECORD                                  
*                                                                               
         LA    R8,DATASET                                                       
         USING IHADCB,R8                                                        
*                                                                               
         B     MAIN                                                             
*                                                                               
XBASE    XBASE                                                                  
EXIT     XIT1  REGS=(R1)                                                        
         EJECT                                                                  
***********************************************************************         
*        INITIALISATION                                               *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
         BAS   RE,OPENALL                                                       
*                                                                               
         BAS   RE,BUILDL                                                        
         L     R2,ABLDLIST                                                      
*                                                                               
MEMBER   XC    ACSECTP,ACSECTP                                                  
         OC    8(3,R2),8(R2)                                                    
         BZ    EOD100                                                           
         CLC   0(8,R2),=C'$$$SPACE'                                             
         BE    EOD100                                                           
         MVC   TTRN(4),8(R2)                                                    
         POINT DATASET,TTRN                                                     
*                                                                               
MAIN010  BAS   RE,READREC                                                       
*                                                                               
         L     R1,AIOAREA                                                       
         CLC   0(4,R1),=X'20800000'                                             
         BNE   MAIN020                                                          
         BAS   RE,BLDTAB                                                        
         B     MAIN010                                                          
*                                                                               
MAIN020  L     R1,ACSECTAB         SORT CSECT TABLE                             
         ICM   RF,15,ACSECTP                                                    
         BZ    EOD100                                                           
         SR    RE,RE                                                            
         SR    RF,R1                                                            
         D     RE,=F'36'                                                        
         GOTO1 =V(XSORT),DMCB,ACSECTAB,(RF),36,4,8                              
         L     RF,ACSECTP                                                       
         MVC   0(R6,RF),FFS                                                     
*                                                                               
         XC    ADISP,ADISP                                                      
         L     R3,ACSECTAB                                                      
*                                                                               
MAIN030  BAS   RE,READREC                                                       
         LH    R1,DCBLRECL         IGNORE IF L' LESS THAN 256                   
         CLI   LOADFLG,C'Y'                                                     
         BE    *+12                                                             
         CH    R1,=H'256'                                                       
         BNH   MAIN035                                                          
         MVI   LOADFLG,C'N'                                                     
         SH    R1,=H'30'           KEEP PREV 30 BYTES                           
         A     R1,AIOAREA                                                       
         L     RF,APREVSV                                                       
         MVC   0(30,RF),0(R1)                                                   
         B     MAIN040                                                          
*                                                                               
MAIN035  L     R1,AIOAREA                                                       
         CLI   0(R1),X'01'                                                      
         BE    *+12                                                             
         CLI   0(R1),X'03'                                                      
         BNE   MAIN030                                                          
*                                                                               
         MVI   LOADFLG,C'Y'                                                     
         CLI   8(R1),X'06'         CHECK/RESET ADISP                            
         BNE   MAIN030             SOMETHING WRONG HERE                         
         MVC   ADISP+1(3),9(R1)                                                 
         B     MAIN030                                                          
*                                                                               
MAIN040  ICM   R1,15,8(R3)                                                      
         SR    RF,RF                                                            
         ICM   RF,7,13(R3)                                                      
         AR    R1,RF                                                            
         C     R1,ADISP                                                         
         BL    MAIN050                                                          
*                                                                               
         L     R1,AIOAREA                                                       
         ICM   RF,15,8(R3)                                                      
         S     RF,ADISP                                                         
         ICM   RE,7,13(R3)         HALF = CSECT LEN                             
         AR    RF,R1                                                            
         AR    RF,RE               POINT RF TO END                              
         LR    RE,RF                                                            
         SR    RE,R1               CHECK IF END > RECLEN                        
         CH    RE,DCBLRECL                                                      
         BH    MAIN060             GET NEXT RECORD                              
*                                                                               
         LR    R1,RF                                                            
         ST    R1,FULL             FULL=END                                     
         MVC   HALF,14(R3)                                                      
         CLC   12(4,R3),=F'256'                                                 
         BL    *+10                                                             
         MVC   HALF,=H'256'                                                     
         SH    R1,HALF                                                          
         ST    R1,FULL                                                          
*                                                                               
         SH    RF,=H'6'                                                         
MAIN041  CLC   0(6,RF),=C'LEVEL='                                               
         BE    MAIN042                                                          
         BCTR  RF,0                TRACK BACK                                   
         C     RF,FULL                                                          
         BH    MAIN041                                                          
         B     MAIN050                                                          
*                                                                               
MAIN042  MVC   16(3,R3),6(RF)      SAVE LEVEL                                   
         MVC   DUB,15(RF)                                                       
*                                                                               
         CLI   DUB+2,C'/'          TEST JAN01/06 FORMAT                         
         BE    MAIN043                                                          
*                                                                               
         BAS   RE,DATCONV2                                                      
*                                                                               
MAIN043  EQU   *                                                                
*&&UK*&& MVC   19(2,R3),DUB+6      REVERSE DATE TO MATCH NY                     
*&&UK*&& MVC   21(2,R3),DUB+3                                                   
*&&UK*&& MVC   23(2,R3),DUB+0                                                   
*                                                                               
         SH    RF,=H'16'                                                        
         CLC   0(5,RF),=C'BOOK='                                                
         BNE   MAIN050                                                          
         MVC   26(10,R3),5(RF)     SAVE BOOK NAME IF THERE                      
*                                                                               
MAIN050  LA    R3,36(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   MAIN040                                                          
*                                                                               
MAIN060  LH    R1,DCBLRECL                                                      
         A     R1,ADISP                                                         
         ST    R1,ADISP                                                         
         B     MAIN030                                                          
*                                                                               
EODATA   EQU   *                                                                
         CLI   DIRBLD,C'Y'                                                      
         BE    BUILDXX                                                          
*                                                                               
         MVC   PLINE,SPACES                                                     
         L     R3,ACSECTAB                                                      
EOD050   MVC   PLINE+0(8),0(R3)    CSECT                                        
*                                                                               
EOD055   OC    19(6,R3),19(R3)     FIND DATE/LVL                                
         BNZ   EOD060                                                           
         LA    R3,36(R3)           USE NEXT CSECT                               
         CLI   0(R3),X'FF'                                                      
         BNE   EOD055                                                           
         SH    R3,=H'36'           UNLESS ITS THE LAST                          
*                                                                               
EOD060   EQU   *                                                                
         MVC   PLINE+9(6),19(R3)   DATE                                         
         MVC   PLINE+18(3),16(R3)  LVL                                          
         MVC   PLINE+22(4),=C'D   '                                             
*&&UK*&& MVC   PLINE+26(2),=C'UK'  COUNTRY                                      
*&&US*&& MVC   PLINE+26(2),=C'US'                                               
         MVC   PLINE+60(8),0(R2)   PHASE                                        
         MVC   PLINE+29(10),26(R3) BOOKNAME                                     
         PUT   PANOUT,PLINE                                                     
         OC    PLINE+1(10),PLINE+1                                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R3,36(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   EOD050                                                           
*                                                                               
EOD100   LA    R2,14(R2)                                                        
         CLC   0(8,R2),FFS                                                      
         BE    EODEOD                                                           
         B     MEMBER                                                           
*                                                                               
EODEOD   BAS   RE,CLOSEALL                                                      
         B     XBASE                                                            
         EJECT                                                                  
*************************************************************                   
*        BUILD TABLE OF CSECTS                              *                   
*************************************************************                   
         SPACE 1                                                                
BLDTAB   NTR1                                                                   
         SR    R3,R3                                                            
         ICM   R3,15,ACSECTP                                                    
         BNZ   *+8                                                              
         L     R3,ACSECTAB                                                      
*                                                                               
         L     RE,AIOAREA                                                       
         SR    R1,R1                                                            
         IC    R1,7(RE)                                                         
         LA    RF,8(RE)                                                         
BLDT010  CLI   8(RF),X'00'         ONLY SAVE TRUE CSECTS                        
         BNE   BLDT015                                                          
         MVC   0(16,R3),0(RF)                                                   
         XC    16(20,R3),16(R3)                                                 
         LA    R3,36(R3)                                                        
BLDT015  LA    RF,16(RF)                                                        
         SH    R1,=H'16'                                                        
         BP    BLDT010                                                          
*                                                                               
         ST    R3,ACSECTP                                                       
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD DIRECTORY LIST                               *                   
*************************************************************                   
         SPACE 1                                                                
BUILDL   NTR1                                                                   
         L     R2,ABLDLIST                                                      
         MVI   DIRBLD,C'Y'                                                      
*                                                                               
BUIL010  BAS   RE,READREC                                                       
         L     R3,AIOAREA                                                       
         LH    R0,0(R3)                                                         
         SH    R0,=H'2'                                                         
         LA    R3,2(R3)                                                         
*                                                                               
BUIL020  XC    0(14,R2),0(R2)                                                   
         MVC   0(11,R2),0(R3)                                                   
         TM    11(R3),X'80'        SKIP IF THIS IS ON                           
         BO    BUIL030                                                          
         TM    10(R3),X'80'        SKIP IF THIS IS ON                           
         BO    BUIL030                                                          
         CLI   0(R3),X'00'         SKIP IF THIS IS ZERO                         
         BE    BUIL030                                                          
         CLI   2(R3),X'00'         SKIP IF THIS IS ZERO                         
         BE    BUIL030                                                          
         LA    R2,14(R2)                                                        
*                                                                               
BUIL030  SR    R1,R1                                                            
         IC    R1,11(R3)                                                        
         N     R1,=X'0000001F'                                                  
         SLL   R1,1                                                             
         LA    R1,12(R1)                                                        
         AR    R3,R1                                                            
         SR    R0,R1                                                            
         LTR   R0,R0                                                            
         BM    BUIL010                                                          
         B     BUIL020                                                          
*                                                                               
BUILDXX  MVC   0(8,R2),FFS                                                      
         MVI   DIRBLD,C'N'                                                      
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        OPEN CLOSE AND READ CLOSE FILES AND EXIT           *                   
*************************************************************                   
         SPACE 1                                                                
OPENALL  ST    RE,SAVERE                                                        
         OPEN  (PANOUT,OUTPUT)   OPEN ALL FILES                                 
         OPEN  (DATASET)                                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
READREC  ST    RE,SAVERE                                                        
         L     RE,AIOAREA                                                       
         READ  DECBX,SF,DATASET,(RE),'S'                                        
         CHECK DECBX                                                            
         L     R1,AIOAREA                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
CLOSEALL ST    RE,SAVERE                                                        
         CLOSE DATASET                                                          
         CLOSE PANOUT                                                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        CONVERT DATES  DUB=01/01/95 TO DUB=01JAN95                   *         
***********************************************************************         
         SPACE 1                                                                
DATCONV  NTR1                                                                   
         TM    DUB+3,X'F0'                                                      
         BNO   DATERR                                                           
         TM    DUB+3,X'F0'                                                      
         BNO   DATERR                                                           
         LA    RF,MONTAB                                                        
         PACK  DUB1,DUB+3(2)                                                    
         CVB   R1,DUB1                                                          
         CH    R1,=H'12'                                                        
         BH    DATERR                                                           
         MH    R1,=H'3'                                                         
         AR    RF,R1                                                            
         MVC   DUB+2(3),0(RF)                                                   
         MVC   DUB+5(2),DUB+6                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CONVERT DATES  DUB=FEB01/06 TO DUB=01/02/06                  *         
***********************************************************************         
         SPACE 1                                                                
DATCONV2 NTR1                                                                   
         LA    R1,MONTAB+3                                                      
         LA    R0,1                                                             
DC2010   CLC   DUB(3),0(R1)        MATCH MONTH                                  
         BE    DC2020                                                           
*                                                                               
         LA    R1,3(R1)                                                         
         AHI   R0,1                                                             
         CLI   0(R1),C'*'          EOT                                          
         BNE   DC2010                                                           
         B     DATERR                                                           
*                                                                               
DC2020   CVD   R0,DUB1             0000000000000012                             
         UNPK  FULL,DUB1                                                        
         L     R1,FULL                                                          
         O     R1,=X'F0F0F0F0'                                                  
         STCM  R1,3,DUB                                                         
         MVI   DUB+2,C'/'                                                       
*                                                                               
*&&UK*&& MVC   FULL(2),DUB         REVERSE UK DATE TO MATCH NY                  
*&&UK*&& MVC   DUB(2),DUB+3                                                     
*&&UK*&& MVC   DUB+3(2),FULL                                                    
         B     EXIT                                                             
*                                                                               
DATERR   MVC   DUB(8),=C'********'                                              
         B     EXIT                                                             
MONTAB   DC    C'***JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC***'                    
         EJECT                                                                  
***********************************************************************         
*        ALLOCATE STORAGE AREA                                        *         
*        ENTRY R1=LENGTH OF AREA IN K.  EXIT R1=31 BIT A(AREA)        *         
***********************************************************************         
         SPACE 1                                                                
GETMAIN  ST    RE,SAVERE                                                        
*                                                                               
         SLL   R1,10               MULTIPLY R1 BY 1K                            
         LR    R0,R1                                                            
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        BLDL LIST                                                    *         
***********************************************************************         
         SPACE 1                                                                
TTRN     DS    F                                                                
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                              *         
***********************************************************************         
         SPACE 1                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
PLINE    DS    CL80                                                             
*                                                                               
DIRBLD   DS    C                                                                
*                                                                               
LOADFLG  DS    C                                                                
*                                                                               
SAVERE   DS    A                                                                
ACSECTAB DS    A                                                                
ABLDLIST DS    A                                                                
AIOAREA  DS    A                                                                
APREVSV  DS    A                                                                
ACSECTP  DC    A(0)                                                             
ADISP    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LTORG                                          *         
***********************************************************************         
         SPACE 1                                                                
FFS      DC    64X'FF'                                                          
SPACES   DC    132C' '                                                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DCBS                                                         *         
***********************************************************************         
         SPACE 1                                                                
DATASET  DCB   DDNAME=DATASET,DSORG=PO,MACRF=R,EODAD=EODATA,RECFM=U,   X        
               LRECL=0                                                          
PANOUT   DCB   DSORG=PS,MACRF=PM,DDNAME=PANOUT,RECFM=FB                         
         EJECT                                                                  
IOAREAQ  EQU   60                  IOAREA 60K                                   
*                                                                               
CSECTQ   EQU   (CSECTN*CSECTL+1023)/1024                                        
CSECTN   EQU   200                                                              
CSECTD   DSECT                                                                  
         DS    CL36                                                             
CSECTL   EQU   *-CSECTD                                                         
*                                                                               
BLDLQ    EQU   (BLDLN*BLDLL+1023)/1024                                          
BLDLN    EQU   10000                                                            
BLDLD    DSECT                                                                  
         DS    CL14                                                             
BLDLL    EQU   *-BLDLD                                                          
*                                                                               
         EJECT                                                                  
         DCBD  DSORG=QS,DEVD=DA                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'165DDCCLIB2  07/17/07'                                      
         END                                                                    
