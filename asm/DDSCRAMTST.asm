*          DATA SET DDSCRAMTST AT LEVEL 001 AS OF 05/19/94                      
*PHASE SCRAMTST                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE SCRAMBLE                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
                                                                                
SCRAMTST CSECT                                                                  
         NBASE WORKX-WORKD,**STST**,WORK=V(REGSAVE),RR=R3                       
         USING WORKD,RC                                                         
         ST    R3,RELO                                                          
                                                                                
         GOTO1 VDMGR,PLIST,DMOPEN,DMSYS,DMFLIST                                 
                                                                                
* Watch this it`s a fiddle.....Used for addressibility into this prog           
         ST    RB,ABASE                                                         
                                                                                
         GOTO1 VSCRAMBL,PLIST,(RC),LEAVEIT,ADDR,(X'C0',CARDGET),       *        
               PRINTOUT                                                         
                                                                                
         GOTO1 VDMGR,PLIST,DMCLSE,DMSYS                                         
         XBASE                                                                  
                                                                                
***********************************************************************         
                                                                                
CARDGET  NTR1                                                                   
         L     RC,0(R1)                                                         
         L     RB,ABASE                                                         
         L     R2,4(R1)                                                         
         LR    R3,R1               Must save R1 before GOTO1                    
         GOTO1 VCARDS,PLIST,(R2),=C'RE00'                                       
         CLC   =C'/*',0(R2)                                                     
                                                                                
         BNE   XIT                                                              
         MVI   0(R3),X'FF'         Indicates EOF to SCRAMBLE                    
XIT      XIT1                                                                   
                                                                                
************************************************************************        
                                                                                
PRINTOUT NTR1                                                                   
         L     RC,0(R1)            Get Back RC                                  
         L     RB,ABASE            Get addressibility                           
         L     R7,VCPRINT                                                       
         USING DPRINT,R7                                                        
         MVC   TITLE(26),=C'Off - Line Script Compiler'                         
         MVC   P(192),SPACES                                                    
         L     R2,4(R1)            Line of I/O script code                      
         MVC   P,0(R2)                                                          
                                                                                
         ZIC   R0,8(R1)            # of errors on this line                     
         L     R2,8(R1)            A(1st CL80 error block)                      
                                                                                
         GOTO1 VPRINTER            Print first I/O line                         
                                                                                
         LTR   R0,R0               Any Errors?                                  
         BZ    XIT                 No                                           
PRIN010  MVC   P(192),SPACES                                                    
         MVC   P(80),0(R2)         Print out an error line                      
         LA    R2,80(R2)                                                        
         GOTO1 VPRINTER                                                         
         BCT   R0,PRIN010          Any more errors                              
         B     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
         LTORG                                                                  
ADDR     DS    0D                                                               
VDMGR    DC    V(DATAMGR)                                                       
VHELLO   DC    V(HELLO)                                                         
VSQUASH  DC    V(SQUASHER)                                                      
                                                                                
VSCRAMBL DC    V(SCRAMBLE)                                                      
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VPRINTER DC    V(PRINTER)                                                       
                                                                                
DMFLIST  DC    C'UCTFILE X'                                                     
DMCLSE   DC    C'DMCLSE'                                                        
DMOPEN   DC    C'OPEN'                                                          
DMSYS    DC    C'CONTROL'                                                       
                                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
PLIST    DS    XL24                                                             
RELO     DS    A                                                                
ABASE    DS    A                                                                
         DS    0D                                                               
LEAVEIT  DS    4096C               Keep your paws off it`s for ASM              
                                                                                
WORKX    DS    0C                                                               
                                                                                
       ++INCLUDE DDDPRINTL                                                      
                                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDSCRAMTST05/19/94'                                      
         END                                                                    
