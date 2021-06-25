*          DATA SET DDCCREP2   AT LEVEL 163 AS OF 03/20/97                      
*PHASE CCREP1,*                                                                 
*INCLUDE DATCON                                                                 
*                                                                               
         PRINT NOGEN                                                            
         SPACE 1                                                                
CCREP1   CSECT                                                                  
         NBASE WORKX-WORKD,XXRMORXX,R9,WORK=A(WORKAREA),CLEAR=YES               
         USING WORKD,RC                                                         
         USING PLINED,SAVELINE                                                  
         SPACE 1                                                                
*                                                                               
INIT01   OPEN  (PANFILE,INPUT)     OPEN THE PAN FILE                            
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,MAIN                                                          
         BAS   RE,PRINTX                                                        
         CLOSE PANFILE             CLOSE PAN FILE                               
*                                                                               
XBASE    XBASE                                                                  
         EJECT                                                                  
*************************************************************                   
*        OPEN OUTPUT DATA SET                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         BAS   RE,PRINTT           PRINT TITLE                                  
         L     R3,=A(INCAREA)                                                   
         ST    R3,AINCA            SET *INC POINTER TO ZERO                     
         MVC   0(6,R3),FFS                                                      
*                                                                               
MAIN010  GET   PANFILE             GET A RECORD                                 
         LR    R2,R1                                                            
*                                                                               
         MVC   DUB,SAVEBDQ(R2)     CONVERT DATE                                 
         MVC   RECTYPE,20(R2)                                                   
         MVC   RECDATE,=C'........'                                             
*                                                                               
         MVC   DUB1(6),DUB         PROTECT DATCON                               
         NC    DUB1(6),=X'F0F0F0F0F0F0'                                         
         CLC   DUB1(6),=X'F0F0F0F0F0F0'                                         
         BNE   MAIN015                                                          
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,DUB),(8,RECDATE)                              
*                                                                               
MAIN015  CLC   SAVCSECT,SAVECQ(R2) TEST CSECT/LEVEL/DATE                        
         BNE   MAIN020                                                          
         CLC   SAVLEVEL,SAVEBLQ(R2)                                             
         BNE   MAIN020                                                          
         CLC   SAVDATE,RECDATE                                                  
         BNE   MAIN020                                                          
         B     MAIN030             ALL SAME AS BEFORE                           
*                                                                               
MAIN020  TM    PFLAG,PFHOLD        PRINT PREVIOUS IF NEC                        
         BNO   MAIN025                                                          
         NI    PFLAG,255-PFHOLD                                                 
*NOP     BAS   RE,FILTER                                                        
         BNE   MAIN010                                                          
         NI    PFLAG,255-PFTOPS                                                 
         MVC   PLINE,SAVELINE                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
MAIN025  CLC   SAVCSECT,SAVECQ(R2)                                              
         BE    MAIN026                                                          
         B     MAIN026                                                          
         NI    PFLAG,255-PFINCS                                                 
         L     R3,=A(INCAREA)                                                   
MAIN025A CLC   0(6,R3),FFS                                                      
         BNE   MAIN025B                                                         
         L     R3,=A(INCAREA)                                                   
         ST    R3,AINCA            SET *INC POINTER TO ZERO                     
         MVC   0(6,R3),FFS                                                      
         L     R3,=A(INCAREA)                                                   
         B     MAIN026                                                          
*                                                                               
MAIN025B OC    0(6,R3),0(R3)                                                    
         BZ    MAIN025D                                                         
         MVC   SAVELINE,BK                                                      
         MVC   PRMBOOK(8),0(R3)                                                 
         MVC   PINCLUD(10),8(R3)                                                
         MVC   PPHASE(8),18(R3)                                                 
         TM    PFLAG,PFINCS                                                     
         BO    MAIN025C                                                         
         BAS   RE,PRINTM                                                        
         OI    PFLAG,PFINCS                                                     
MAIN025C MVC   PLINE,SAVELINE                                                   
         BAS   RE,PRINTL                                                        
MAIN025D LA    R3,26(R3)                                                        
         B     MAIN025A                                                         
*                                                                               
MAIN026  TM    PFLAG,PFTOPS                                                     
         BO    *+8                                                              
         BAS   RE,PRINTM           PRINT MID LINE                               
         OI    PFLAG,PFTOPS                                                     
         MVC   SAVELINE,BK         INIT PRINT LINE                              
*                                                                               
         MVC   SAVCSECT,SAVECQ(R2) SAVE DETAILS                                 
         MVC   SAVLEVEL,SAVEBLQ(R2)                                             
         MVC   SAVDATE,RECDATE                                                  
*                                                                               
         CLI   20(R2),C'C'         IF C "INCLUDE BOOK" CLEAR TABLE              
         BNE   MAIN030                                                          
         L     R3,=A(INCAREA)                                                   
         ST    R3,AINCA            SET *INC POINTER TO ZERO                     
         MVC   0(6,R3),FFS                                                      
*                                                                               
MAIN030  CLI   20(R2),C'A'         IF A "PAN BOOK" SAVE DETAIL                  
         BNE   MAIN051                                                          
         MVC   PBOOK(10),SAVEBQ(R2)                                             
         MVC   PBLEVEL(3),SAVEBLQ(R2)                                           
         MVC   PBDATE(7),RECDATE                                                
         MVC   PCSECT(8),SAVECQ(R2)                                             
         MVC   PRMBOOK(8),SAVERQ(R2)                                            
         OI    PFLAG,PFHOLD        FLAG THIS TO PRINT IF NECCESARY              
         B     MAIN010                                                          
*                                                                               
MAIN051  CLI   20(R2),C'B'         IF B "RMBOOK" SAVE MORE DETAIL               
         BNE   MAIN052                                                          
         MVC   PBLEVEL(3),SAVEBLQ(R2)                                           
         MVC   PBDATE(7),RECDATE                                                
         MVC   PCSECT(8),SAVECQ(R2)                                             
         MVC   PRMBOOK(8),SAVERQ(R2)                                            
         OI    PFLAG,PFHOLD        FLAG THIS TO PRINT IF NECCESARY              
         B     MAIN010                                                          
*                                                                               
MAIN052  CLI   20(R2),C'C'         IF C "INCLUDE BOOK"                          
         BNE   MAIN053                                                          
         L     R3,AINCA            UPDATE INCLUDE TABLE                         
         MVC   0(8,R3),SAVERQ(R2)                                               
         MVC   8(10,R3),SAVEIQ(R2)                                              
         MVC   18(8,R3),SAVEPQ(R2)                                              
         LA    R3,26(R3)           RMBOOK..INCLUDE...PHASE...                   
         MVC   0(6,R3),FFS                                                      
         ST    R3,AINCA                                                         
         B     MAIN010                                                          
*                                                                               
MAIN053  CLI   20(R2),C'D'         IF D "PHASE" BOOK                            
         BNE   MAIN055                                                          
         MVC   PBLEVEL(3),SAVEBLQ(R2)                                           
         MVC   PBDATE(7),RECDATE                                                
         MVC   PCSECT(8),SAVECQ(R2)                                             
         MVC   PPHASE(8),SAVEPQ(R2)                                             
*                                                                               
         L     R3,=A(INCAREA)      SEARCH TABLE                                 
MAIN053A CLC   0(4,R3),FFS                                                      
         BE    MAIN054                                                          
         CLC   PPHASE(8),18(R3)    SAME PHASE                                   
         BE    *+12                                                             
         LA    R3,26(R3)                                                        
         B     MAIN053A            KEEP TRYING                                  
*                                                                               
         MVC   PINCLUD(10),8(R3)   COPY AND CLEAR IT                            
         XC    0(26,R3),0(R3)                                                   
*                                                                               
MAIN054  NI    PFLAG,255-PFHOLD                                                 
         B     MAIN060             FILL IN AND PRINT THIS ONE                   
*                                                                               
MAIN055  B     MAIN010                                                          
*                                                                               
MAIN060  TM    PFLAG,PFTOPS                                                     
         B     MAIN065                                                          
*                                                                               
         MVC   PBLEVEL(3),SPACES                                                
         MVC   PBDATE(7),SPACES                                                 
         MVC   PCSECT(8),SPACES                                                 
*                                                                               
MAIN065  BAS   RE,FILTER                                                        
         BNE   MAIN010                                                          
         NI    PFLAG,255-PFTOPS                                                 
         MVC   PLINE,SAVELINE                                                   
         BAS   RE,PRINTL                                                        
         B     MAIN010                                                          
*                                                                               
PANXX    EQU   *                                                                
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        FILTER                                             *                   
*************************************************************                   
         SPACE 1                                                                
FILTER   NTR1                                                                   
*                                                                               
         CLC   PPHASE(3),=C'FAC'                                                
         BE    FILTERY                                                          
         B     FILTERN                                                          
*                                                                               
FILTERN  LTR   RB,RB                                                            
         B     XIT1                                                             
FILTERY  CR    RB,RB                                                            
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        REPORT LINES                                       *                   
*************************************************************                   
         SPACE 1                                                                
T1       DS    CL166                                                            
         ORG   T1                                                               
         DC    C'1'                                                             
         DC    AL1(TL),15AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),10AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TR)                                                          
         DC    32C' '                                                           
         ORG                                                                    
*                                                                               
T2       DS    CL166                                                            
         ORG   T2                                                               
         DC    C' '                                                             
         DC    AL1(VB),CL15'SOURCE BOOK'                                        
         DC    AL1(VB),CL05'LVL'                                                
         DC    AL1(VB),CL10'DATE'                                               
         DC    AL1(VB),CL15'CSECT'                                              
         DC    AL1(VB),CL15'RMBOOK'                                             
         DC    AL1(VB),CL15'*INCLUDE'                                           
         DC    AL1(VB),CL15'PHASE'                                              
         DC    AL1(VB)                                                          
         DC    32C' '                                                           
         ORG                                                                    
*                                                                               
BK       DS    CL166                                                            
         ORG   BK                                                               
         DC    C' '                                                             
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),10AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB)                                                          
         DC    32C' '                                                           
         ORG                                                                    
*                                                                               
M1       DS    CL166                                                            
         ORG   M1                                                               
         DC    C' '                                                             
         DC    AL1(ML),15AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),10AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MR)                                                          
         DC    32C' '                                                           
         ORG                                                                    
*                                                                               
B1       DS    CL166                                                            
         ORG   B1                                                               
         DC    C' '                                                             
         DC    AL1(BL),15AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),10AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BR)                                                          
         DC    32C' '                                                           
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        BOX EQUATES                                        *                   
*************************************************************                   
         SPACE 1                                                                
TL       EQU   X'AC'               TOP LEFT                                     
TM       EQU   X'CC'               TOP MIDDLE                                   
TR       EQU   X'BC'               TOP RIGHT                                    
HB       EQU   X'BF'               HORIZONTAL BAR                               
VB       EQU   X'FA'               VERTICAL  BAR                                
ML       EQU   X'EB'               MIDDLE LEFT                                  
MM       EQU   X'8F'               MIDDLE MIDDLE                                
MR       EQU   X'EC'               MIDDLE RIGHT                                 
BL       EQU   X'AB'               BOTTOM LEFT                                  
BM       EQU   X'CB'               BOTTOM MIDDLE                                
BR       EQU   X'BB'               BOTTOM RIGHT                                 
BB       EQU   X'40'               BLANK LINE                                   
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
PRINTT1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
*                                                                               
         PUT   SYSPRINT,T1         PRINT TITLES                                 
         PUT   SYSPRINT,T2                                                      
         PUT   SYSPRINT,M1                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTM   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BNL   PRINTT1                                                          
*                                                                               
         PUT   SYSPRINT,M1         PRINT MIDLINE                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
         ZAP   LINE,=P'4'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
*                                                                               
         PUT   SYSPRINT,T1         PRINT TITLES                                 
         PUT   SYSPRINT,T2                                                      
         PUT   SYSPRINT,M1                                                      
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*   DATA CONTROL BLOCKS                                     *                   
*************************************************************                   
         SPACE 1                                                                
PANFILE  DCB   DDNAME=PANFILE,DSORG=PS,MACRF=GL,EODAD=PANXX                     
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(133)          
*                                                                               
*        NOTE  RECORD LEN=166 WITH CHARS=(BX15)                                 
*                                                                               
*************************************************************                   
*        LITERALS                                           *                   
*************************************************************                   
         SPACE 1                                                                
SPACES   DC    166C' '                                                          
FFS      DC    26X'FF'                                                          
MAXLINE  DC    PL4'60'                                                          
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WORK AREA                                          *                   
*************************************************************                   
         SPACE 1                                                                
WORKAREA DC  1000D'0'                                                           
         EJECT                                                                  
*************************************************************                   
*        *INCLUDE AREA  (BIG ENOUGH FOR 1500 INCLUDES)      *                   
*************************************************************                   
         SPACE 1                                                                
INCAREA  DC    1500XL26'00'                                                     
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
PLINE    DS    CL166                                                            
SAVELINE DS    CL166                                                            
*                                                                               
WORK     DS    CL166                                                            
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
SAVERE   DS    F                                                                
HALF     DS    H                                                                
DMCB     DS    6F                                                               
*                                                                               
AINCA    DS    A                                                                
*                                                                               
RECDATE  DS    CL7                                                              
RECTYPE  DS    CL1                                                              
LINE     DS    PL4                                                              
PAGE     DS    PL4                                                              
PFLAG    DS    X                                                                
PFHOLD   EQU   X'80'                                                            
PFTOPS   EQU   X'40'                                                            
PFINCS   EQU   X'20'                                                            
*                                                                               
SAVCSECT DS    CL8                                                              
SAVLEVEL DS    CL3                                                              
SAVDATE  DS    CL7                                                              
SAVBOOK  DS    CL10                                                             
*                                                                               
SAVEA    DS    CL80                                                             
SAVEB    DS    CL80                                                             
*                                                                               
SAVECQ   EQU   00,8,C'C'                                                        
SAVEBDQ  EQU   09,6,C'C'                                                        
SAVEBLQ  EQU   16,3,C'C'                                                        
SAVETYQ  EQU   22,1,C'C'                                                        
SAVEBQ   EQU   27,10,C'C'                                                       
SAVERQ   EQU   38,8,C'C'                                                        
SAVEIQ   EQU   47,10,C'C'                                                       
SAVEPQ   EQU   58,8,C'C'                                                        
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
         DS    CL1                                                              
PBOOK    DS    CL15                                                             
         DS    CL1                                                              
PBLEVEL  DS    CL05                                                             
         DS    CL1                                                              
PBDATE   DS    CL10                                                             
         DS    CL1                                                              
PCSECT   DS    CL15                                                             
         DS    CL1                                                              
PRMBOOK  DS    CL15                                                             
         DS    CL1                                                              
PINCLUD  DS    CL15                                                             
         DS    CL1                                                              
PPHASE   DS    CL15                                                             
         DS    CL1                                                              
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'163DDCCREP2  03/20/97'                                      
         END                                                                    
