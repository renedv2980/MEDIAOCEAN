*          DATA SET DDCCREP3   AT LEVEL 142 AS OF 03/20/97                      
*PHASE CCREP3,*                                                                 
*INCLUDE DATCON                                                                 
         PRINT NOGEN                                                            
         SPACE 1                                                                
CCREP3   CSECT                                                                  
         NBASE WORKX-WORKD,XXRMORXX,R9,WORK=A(WORKAREA),CLEAR=YES               
         USING WORKD,RC                                                         
         USING PLINED,PLINE                                                     
         SPACE 1                                                                
*                                  INITIALISE STORAGE                           
INIT01   OPEN  (PANFILE,INPUT)                                                  
         BAS   RE,PRINTI                                                        
         BAS   RE,MAIN                                                          
         BAS   RE,PRINTX                                                        
         CLOSE PANFILE                                                          
*                                                                               
XBASE    XBASE                                                                  
         EJECT                                                                  
*************************************************************                   
*        FILL SAVED BLOCK WITH PAN FILE INFO                *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         BAS   RE,PRINTT           PRINT TITLE                                  
*                                                                               
MAIN010  GET   PANFILE             GET A LINE                                   
         LR    R2,R1                                                            
*                                                                               
         CLC   0(8,R2),CSECTSAV    RESET BLOCK ON NEW CSECT                     
         BE    MAIN015                                                          
         LA    R3,SAVEBLK                                                       
         ST    R3,ANEXTIN                                                       
         BAS   RE,OUTBLK           GO PROCESS THIS BLOCK                        
*                                                                               
MAIN015  MVC   CSECTSAV,0(R2)                                                   
         L     R3,ANEXTIN          SAVE THIS ENTRY                              
         MVC   0(65,R3),0(R2)                                                   
         MVI   65(R3),0                                                         
         USING PANLINED,R3                                                      
*                                                                               
         MVC   DUB,PANDATE(R2)     CONVERT DATE TO RECDATE                      
         MVC   PANDATE1,=C'........'                                            
*                                                                               
         MVC   DUB1(6),DUB         USE THIS TO PROTECT DATCON                   
         NC    DUB1(6),=X'F0F0F0F0F0F0'                                         
         CLC   DUB1(6),=X'F0F0F0F0F0F0'                                         
         BNE   MAIN010                                                          
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,DUB),(8,PANDATE1)                             
         B     MAIN010                                                          
         EJECT                                                                  
************************************************************                    
*        OUTPUT SAVED BLOCK TO PRINT                       *                    
************************************************************                    
         SPACE 1                                                                
OUTBLK   NTR1                                                                   
OUTBLKX  XIT1                                                                   
         EJECT                                                                  
MAIN030  XC    PRSAVE,PRSAVE??                                                  
         BNE   MAIN040                                                          
         CLC   PRELEVEL,SAVEBLQ(R2)                                             
         BNE   MAIN040                                                          
         CLC   PREDATE,RECDATE                                                  
         BNE   MAIN040                                                          
         B     MAIN050                                                          
*                                                                               
MAIN040  MVC   PBOOK(10),SPACES                                                 
         BAS   RE,FILTER                                                        
         BNE   MAIN050                                                          
         CLC   PPHASE,SPACES                                                    
         BNE   MAIN050                                                          
         B     MAIN050                                                          
*                                                                               
         CLI   SAVELINE,0          NOP THIS CRAP                                
         BE    *+10                                                             
         CLC   PLINE+17(32),SAVELINE+17                                         
         BE    *+8                                                              
         BAS   RE,PRINTM                                                        
         MVC   SAVELINE,PLINE                                                   
         BAS   RE,PRINTL                                                        
         MVC   PLINE,BK                                                         
*                                                                               
MAIN050  MVC   PRECSECT,SAVECQ(R2)                                              
         MVC   PRELEVEL,SAVEBLQ(R2)                                             
         MVC   PREDATE,RECDATE                                                  
*                                                                               
         CLI   20(R2),C'A'                                                      
         BNE   MAIN051                                                          
         MVC   PBOOK(10),SAVEBQ(R2)                                             
         MVC   PBLEVEL(3),SAVEBLQ(R2)                                           
         MVC   PBDATE(7),RECDATE                                                
         MVC   PCSECT(8),SAVECQ(R2)                                             
         MVC   PRMBOOK(8),SAVERQ(R2)                                            
         MVC   PPHASE(8),SAVEPQ(R2)                                             
         B     MAIN060                                                          
*                                                                               
MAIN051  CLI   20(R2),C'B'                                                      
         BNE   MAIN053                                                          
         MVC   PBLEVEL(3),SAVEBLQ(R2)                                           
         MVC   PBDATE(7),RECDATE                                                
         MVC   PCSECT(8),SAVECQ(R2)                                             
         MVC   PRMBOOK(8),SAVERQ(R2)                                            
         B     MAIN060                                                          
*                                                                               
MAIN053  CLI   20(R2),C'D'                                                      
         BNE   MAIN054                                                          
         MVC   PBLEVEL(3),SAVEBLQ(R2)                                           
         MVC   PBDATE(7),RECDATE                                                
         MVC   PCSECT(8),SAVECQ(R2)                                             
         MVC   PINCLUD(10),SAVEIQ(R2)                                           
         MVC   PPHASE(8),SAVEPQ(R2)                                             
         BAS   RE,FILTER                                                        
         BNE   MAIN060                                                          
*                                                                               
         CLI   SAVELINE,0                                                       
         BE    *+10                                                             
         CLC   PLINE+17(32),SAVELINE+17                                         
         BE    *+8                                                              
         BAS   RE,PRINTM                                                        
         MVC   SAVELINE,PLINE                                                   
         BAS   RE,PRINTL                                                        
         MVC   PLINE,BK                                                         
*                                                                               
         B     MAIN060                                                          
*                                                                               
MAIN054  EQU   *                                                                
*                                                                               
MAIN060  B     MAIN010                                                          
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
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),10AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),10AL1(HB)                                                
         DC    AL1(TR)                                                          
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
         DC    AL1(VB),CL05'LVL'                                                
         DC    AL1(VB),CL10'DATE'                                               
         DC    AL1(VB),CL15'*INCLUDE'                                           
         DC    AL1(VB),CL15'PHASE'                                              
         DC    AL1(VB),CL05'LVL'                                                
         DC    AL1(VB),CL10'DATE'                                               
         DC    AL1(VB)                                                          
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
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),10AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),10AL1(BB)                                                
         DC    AL1(VB)                                                          
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
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),10AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),10AL1(HB)                                                
         DC    AL1(MR)                                                          
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
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),10AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),10AL1(HB)                                                
         DC    AL1(BR)                                                          
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
*        MVC   PLINE,SPACES                                                     
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
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
*        NOTE  RECORD LEN=166 WITH CHARS=(BX15)                                 
*                                                                               
*************************************************************                   
*        LITERALS                                           *                   
*************************************************************                   
         SPACE 1                                                                
SPACES   DC    166C' '                                                          
MAXLINE  DC    PL4'60'                                                          
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WORK AREA                                          *                   
*************************************************************                   
         SPACE 1                                                                
WORKAREA DC  10000D'0'                                                          
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
PLINE    DS    CL166                                                            
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
SAVERE   DS    F                                                                
HALF     DS    H                                                                
DMCB     DS    6F                                                               
*                                                                               
LINE     DS    PL4                                                              
PAGE     DS    PL4                                                              
*                                                                               
SAVEBLK  DS    1000CL65                                                         
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
PRLEVEL  DS    CL05                                                             
         DS    CL1                                                              
PRDATE   DS    CL10                                                             
         DS    CL1                                                              
PINCLUD  DS    CL15                                                             
         DS    CL1                                                              
PPHASE   DS    CL15                                                             
         DS    CL1                                                              
PPLEVEL  DS    CL05                                                             
         DS    CL1                                                              
PPDATE   DS    CL10                                                             
         DS    CL1                                                              
         EJECT                                                                  
PANLINED DSECT                                                                  
PANLINE  DS    0CL70                                                            
PANCSECT DS    CL8                                                              
PANDATE1 DS    0CL8                                                             
         DS    CL1                                                              
PANDATE  DS    CL8                                                              
         DS    CL1                                                              
PANLEVEL DS    CL3                                                              
         DS    CL1                                                              
PANTYPE  DS    CL6                                                              
         DS    CL1                                                              
PANBOOK  DS    CL10                                                             
         DS    CL1                                                              
PANRMBK  DS    CL10                                                             
         DS    CL1                                                              
PANLMBK  DS    CL10                                                             
         DS    CL1                                                              
PANPHASE DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'142DDCCREP3  03/20/97'                                      
         END                                                                    
