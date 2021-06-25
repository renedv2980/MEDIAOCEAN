*          DATA SET DTPERVAL   AT LEVEL 017 AS OF 07/09/19                      
*PHASE DTPERVAL                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE PERVALO                                                                
*INCLUDE PERVALN                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'TEST PERVAL'                                                    
**********************************************************************          
* THIS VERSION RELAYS ON A RM OF THE OLD, LIVE AND NEW VERSION                  
* YOU CAN REPORT 1, 2 OR ALL 3 VALUES AT ONCE. THAT IS                          
* LIVE VERSION, OLD VERSION AND NEW VERSION                                     
* RUNS THROUGH 4 ITERATIONS DATVAL'S                                            
* P1(1) = 1 MM/DD/YY                                                            
*         2 MM/DD                                                               
*         3 MM/YY                                                               
*         4 DAY                                                                 
*                                                                               
* FOR EXAMPLE                                                                   
* O 80       <-- X'80' = OLD VERSION, DISPLAY DATE AS NON DDS FORMAT            
* SPECIFY IN COLUMN 1 IF YOU WANT DATVAL VERSION O=OLD, N=NEW, L=LIVE           
* SPECIFY ON SAME LINE WHAT THE P2(1) VALUES ARE                                
*                                                                               
* RUNS THOUGH DATE VARIATIONS STARTING IN COLUMN 3                              
*                                                                               
* FOR EXAMPLE                                                                   
*   10/1/18                                                                     
*                                                                               
**********************************************************************          
         PRINT  GEN                                                             
TEST     CSECT                                                                  
         NBASE 0,*TPERVL*,WORK                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(28),=CL28'  TEST PERVAL SUBROUTINE'                        
         LA    R4,MID1                                                          
         MVI   P1LVAL,0                                                         
         MVI   P2LVAL,0                                                         
         MVI   PXLVAL,0                                                         
         MVI   P1OVAL,0                                                         
         MVI   P2OVAL,0                                                         
         MVI   PXOVAL,0                                                         
         MVI   P1NVAL,0                                                         
         MVI   P2NVAL,0                                                         
         MVI   PXNVAL,0                                                         
         MVC   P1LVALC,=C'00'                                                   
         MVC   P2LVALC,=C'00'                                                   
         MVC   PXLVALC,=C'00'                                                   
         MVC   P1OVALC,=C'00'                                                   
         MVC   P2OVALC,=C'00'                                                   
         MVC   PXOVALC,=C'00'                                                   
         MVC   P1NVALC,=C'00'                                                   
         MVC   P2NVALC,=C'00'                                                   
         MVC   PXNVALC,=C'00'                                                   
*                                                                               
         USING PVALD,R4                                                         
         MVC   PINPUT(12),=CL12'INPUT DATE'                                     
         MVC   PPARM1,=C'P1'                                                    
         MVC   PPARM2,=C'P2'                                                    
         MVC   PPARMX,=C'PX'                                                    
         MVC   PRTN,=C'RC'                                                      
         MVC   PNCOMPS(8),=CL8'NEW COMP'                                        
         MVC   PBINRYS(6),=CL5'BINARY'                                          
         MVC   POCOMPS(8),=CL8'OLD COMP'                                        
         MVC   PPWOS#S(4),=CL4'PWOS'                                            
*&&US*&& MVC   PYYMMDS(6),=CL6'YYMMDD'                                          
*&&UK*&& MVC   PYYMMDS(6),=CL6'YYMMDD'                                          
         DROP  R4                                                               
*                                                                               
NEXT     GOTO1 =V(CARDS),PARAM,C,=C'RE00'                                       
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
*                                                                               
NEXT02   MVC   ACTION,C            T DATE                                       
         MVI   BYTE,0                                                           
         XC    PARAM(24),PARAM                                                  
         CLI   ACTION,C'*'         PRINT COMMENT CARD                           
         BE    NEXTCARD                                                         
         CLI   ACTION,C'L'         P1(1)= VALUE LIVE                            
         BNE   NEXT04                                                           
         MVC   P1LVALC,C+2                                                      
         GOTO1 =V(HEXIN),DMCB,P1LVALC,P1LVAL,2,0                                
         MVC   P2LVALC,C+5                                                      
         GOTO1 =V(HEXIN),DMCB,P2LVALC,P2LVAL,2,0                                
         MVC   PXLVALC,C+8                                                      
         GOTO1 =V(HEXIN),DMCB,PXLVALC,PXLVAL,2,0                                
*                                                                               
NEXT04   CLI   ACTION,C'O'         P1(1)= VALUE OLD                             
         BNE   NEXT06                                                           
         MVC   P1OVALC,C+2                                                      
         GOTO1 =V(HEXIN),DMCB,P1OVALC,P1OVAL,2,0                                
         MVC   P2OVALC,C+5                                                      
         GOTO1 =V(HEXIN),DMCB,P2OVALC,P2OVAL,2,0                                
         MVC   PXOVALC,C+8                                                      
         GOTO1 =V(HEXIN),DMCB,PXOVALC,PXOVAL,2,0                                
*                                                                               
NEXT06   CLI   ACTION,C'N'         P1(1)= VALUE NEW                             
         BNE   NEXT10                                                           
         MVC   P1NVALC,C+2                                                      
         GOTO1 =V(HEXIN),DMCB,P1NVALC,P1NVAL,2,0                                
         MVC   P2NVALC,C+5                                                      
         GOTO1 =V(HEXIN),DMCB,P2NVALC,P2NVAL,2,0                                
         MVC   PXNVALC,C+8                                                      
         GOTO1 =V(HEXIN),DMCB,PXNVALC,PXNVAL,2,0                                
*                                                                               
NEXT10   LA    R4,P                                                             
         USING PVALD,R4                                                         
         CLI   ACTION,C' '         IF BLANK DO 0 TO 3                           
         BNE   NEXTCARD                                                         
         MVC   PINPUT,C+2          PRINT ONCE                                   
         MVC   PERIOD,C+2          SAVE FOR EACH PERVAL CALL                    
                                                                                
**********************************************************************          
*  P2=X'00'    LIVE                                                             
**********************************************************************          
         LA    R2,PERBLK                                                        
         USING PERVALD,R2                                                       
         CLI   P1LVAL,X'FF'                                                     
         BE    PROCOLD                                                          
         XC    PERBLK,PERBLK                                                    
         MVI   PVER,C'L'                                                        
         MVC   PPARM1,P1LVALC                                                   
         MVC   PPARM2,P2LVALC                                                   
         MVC   PPARMX,PXLVALC                                                   
         MVC   PVALIND1,PXLVAL                                                  
         GOTOR =V(PERVAL),PARAM,(P1LVAL,PERIOD),(P2LVAL,PERBLK)                 
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN,1,0                                 
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALNSTA,PNCOMPS,2,0   NEW COMPRESSED            
         GOTOR =V(HEXOUT),DMCB,PVALNEND,PNCOMPE,2,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALBSTA,PBINRYS,3,0   BINARY                    
         GOTOR =V(HEXOUT),DMCB,PVALBEND,PBINRYE,3,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALCSTA,POCOMPS,2,0   OLD COMPRESSED            
         GOTOR =V(HEXOUT),DMCB,PVALCEND,POCOMPE,2,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALPSTA,PPWOS#S,3,0   PWOS                      
         GOTOR =V(HEXOUT),DMCB,PVALPEND,PPWOS#E,3,0                             
*                                                                               
         MVC   PYYMMDS,PVALESTA                       C'YYMMDD'                 
         MVC   PYYMMDE,PVALEEND                                                 
*                                                                               
         GOTOR =V(PRINTER)                                                      
                                                                                
**********************************************************************          
*  P2=X'00'    OLD                                                              
**********************************************************************          
PROCOLD  CLI   P1OVAL,X'FF'                                                     
         BE    PROCNEW                                                          
         XC    PERBLK,PERBLK                                                    
         MVI   PVER,C'O'                                                        
         MVC   PPARM1,P1OVALC                                                   
         MVC   PPARM2,P2OVALC                                                   
         MVC   PPARMX,PXOVALC                                                   
         MVC   PVALIND1,PXOVAL                                                  
         GOTOR =V(PERVALO),PARAM,(P1OVAL,PERIOD),(P2OVAL,PERBLK)                
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN,1,0                                 
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALNSTA,PNCOMPS,2,0   NEW COMPRESSED            
         GOTOR =V(HEXOUT),DMCB,PVALNEND,PNCOMPE,2,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALBSTA,PBINRYS,3,0   BINARY                    
         GOTOR =V(HEXOUT),DMCB,PVALBEND,PBINRYE,3,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALCSTA,POCOMPS,2,0   OLD COMPRESSED            
         GOTOR =V(HEXOUT),DMCB,PVALCEND,POCOMPE,2,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALPSTA,PPWOS#S,3,0   PWOS                      
         GOTOR =V(HEXOUT),DMCB,PVALPEND,PPWOS#E,3,0                             
*                                                                               
         MVC   PYYMMDS,PVALESTA                       C'YYMMDD'                 
         MVC   PYYMMDE,PVALEEND                                                 
*                                                                               
         GOTOR =V(PRINTER)                                                      
                                                                                
**********************************************************************          
*  P2=X'00'    NEW                                                              
**********************************************************************          
PROCNEW  CLI   P1NVAL,X'FF'                                                     
         BE    NEXT                                                             
         XC    PERBLK,PERBLK                                                    
         MVI   PVER,C'N'                                                        
         MVC   PPARM1,P1NVALC                                                   
         MVC   PPARM2,P2NVALC                                                   
         MVC   PPARMX,PXNVALC                                                   
         MVC   PVALIND1,PXNVAL                                                  
         GOTOR =V(PERVALN),PARAM,(P1NVAL,PERIOD),(P2NVAL,PERBLK)                
         GOTOR =V(HEXOUT),DMCB,PARAM+4,PRTN,1,0                                 
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALNSTA,PNCOMPS,2,0   NEW COMPRESSED            
         GOTOR =V(HEXOUT),DMCB,PVALNEND,PNCOMPE,2,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALBSTA,PBINRYS,3,0   BINARY                    
         GOTOR =V(HEXOUT),DMCB,PVALBEND,PBINRYE,3,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALCSTA,POCOMPS,2,0   OLD COMPRESSED            
         GOTOR =V(HEXOUT),DMCB,PVALCEND,POCOMPE,2,0                             
*                                                                               
         GOTOR =V(HEXOUT),DMCB,PVALPSTA,PPWOS#S,3,0   PWOS                      
         GOTOR =V(HEXOUT),DMCB,PVALPEND,PPWOS#E,3,0                             
*                                                                               
         MVC   PYYMMDS,PVALESTA                       C'YYMMDD'                 
         MVC   PYYMMDE,PVALEEND                                                 
*                                                                               
         GOTOR =V(PRINTER)                                                      
*                                                                               
NEXTCARD GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
*                                                                               
ERROR    DS    0H                                                               
*                                                                               
EXIT     XBASE                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
P1LVAL   DS    X                                                                
P2LVAL   DS    X                                                                
PXLVAL   DS    X                                                                
*                                                                               
P1OVAL   DS    X                                                                
P2OVAL   DS    X                                                                
PXOVAL   DS    X                                                                
*                                                                               
P1NVAL   DS    X                                                                
P2NVAL   DS    X                                                                
PXNVAL   DS    X                                                                
*                                                                               
P1LVALC  DS    CL2                                                              
P2LVALC  DS    CL2                                                              
PXLVALC  DS    CL2                                                              
*                                                                               
P1OVALC  DS    CL2                                                              
P2OVALC  DS    CL2                                                              
PXOVALC  DS    CL2                                                              
*                                                                               
P1NVALC  DS    CL2                                                              
P2NVALC  DS    CL2                                                              
PXNVALC  DS    CL2                                                              
*                                                                               
PERIOD   DS    CL20                                                             
*                                                                               
         DS    CL2                                                              
PARAM    DS    6F                                                               
DMCB     DS    6F                                                               
SAVE     DS    20F                                                              
*                                                                               
C        DS    CL80                                                             
         ORG   C                                                                
CDATE    DS    CL8                 DATE (ANY DATVAL FORMAT)                     
         DS    C                                                                
CTH      DS    CL2                 HOUR                                         
CTM      DS    CL2                 MINUTE                                       
CSIGN    DS    C                   SET TO - IF WANT BACKWARDS                   
CHOURS   DS    CL5                 NUMBER OF HOURS TO RETAIN                    
         DS    C                                                                
CCTRY    DS    CL2                 COUNTRY CODE                                 
         DS    C                                                                
CHOL     DS    CL1                 SET TO H IF WANT OFFICIAL HOLIDAY            
         DS    C                                                                
CTALENT  DS    CL1                 SET TO T/U IF WANT TALENT CALENDARS          
         DS    C                                                                
CCALC    DS    CL1                 SET TO C IF WANT CALCULATE                   
         DS    C                                                                
CX       EQU   *                                                                
         ORG                                                                    
*                                                                               
PERBLK   DS    XL(PERVLNQ)                                                      
RETC     DS    X                                                                
CTRY     DS    X                                                                
ACTION   DS    C                                                                
HRS      DS    XL2                                                              
FLAG     DS    XL1                                                              
RETI     DS    XL1                                                              
IDY      DS    X                                                                
IDM      DS    X                                                                
IDD      DS    X                                                                
ITH      DS    X                                                                
ITM      DS    X                                                                
ODY      DS    X                                                                
ODM      DS    X                                                                
ODD      DS    X                                                                
OTH      DS    X                                                                
OTM      DS    X                                                                
*                                                                               
WORK     DS    1000D                                                            
         EJECT                                                                  
PVALD    DSECT                                                                  
PINPUT   DS   CL20                                                              
         DS   C                                                                 
PVER     DS   C                    VERSION (L,N,O)                              
         DS   C                                                                 
PPARM1   DS   CL2                  PARM 1                                       
         DS   C                                                                 
PPARM2   DS   CL2                  PARM 2                                       
         DS   C                                                                 
PPARMX   DS   CL2                  PARM EXTENDED                                
         DS   C                                                                 
PRTN     DS   CL2                  RETURN CODE FROM PERVAL                      
         DS   C                                                                 
PNCOMPS  DS   CL4                  NEW COMPRESSED START                         
         DS   C                                                                 
PNCOMPE  DS   CL4                  NEW COMPRESSED END                           
         DS   C                                                                 
PBINRYS  DS   CL6                  NEW BINARY START                             
         DS   C                                                                 
PBINRYE  DS   CL6                  NEW BINARY END                               
         DS   C                                                                 
POCOMPS  DS   CL4                  OLD COMPRESSED START                         
         DS   C                                                                 
POCOMPE  DS   CL4                  OLD COMPRESSED END                           
         DS   C                                                                 
PPWOS#S  DS   CL6                  PWOS START                                   
         DS   C                                                                 
PPWOS#E  DS   CL6                  PWOS END                                     
         DS   C                                                                 
PYYMMDS  DS   CL6                  YYMMDD START                                 
         DS   C                                                                 
PYYMMDE  DS   CL6                  YYMMDD END                                   
         DS   C                                                                 
PVALLNQ  EQU   PVALD                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017DTPERVAL  07/09/19'                                      
         END                                                                    
