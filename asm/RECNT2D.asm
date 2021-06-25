*          DATA SET RECNT2D    AT LEVEL 005 AS OF 04/14/05                      
*PHASE T8022DC                                                                  
*INCLUDE OUTDAY                                                                 
         TITLE 'T8022D - REPPAK CONTRACT GROUP/SUBGROUP CHANGE'                 
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT2D (T8022D) --- CONTRACT QUICK FIX FOR GRP/SUBGRP   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 21JUN93 (SKU) DATE OF ORIGIN                                    *             
*               THIS PROGRAM WILL: 1) CHANGE THE GROUP/SUBGROUP   *             
*               CODE IN THE CONTRACT RECORD, 2) CREATE NEW X'0C'  *             
*               AND X'9C' KEYS WITH THE NEW GROUP/SUBGROUP CODES, *             
*               3) DELETE OLD X'0C' AND X'9C' KEYS WITH OLD GROUP *             
*               /SUBGROUP CODES.                                  *             
*               -> KCHG IN BUYACT, GROUP/SUBGROUP CODE IN BUYNUM  *             
*                                                                 *             
* 06OCT95 (SKU) 2K CONTRACT SUPPORT                               *             
* 25JUL01 (BU ) NEW SCREEN FIELDS                                 *             
* 14APR05 (HQ ) VALIDATE GROUP, ALLOW ONE CHAR GROUP NAME         *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8022D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8022D                                                         
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         LA    R2,CONBNUMH                                                      
         LA    R3,389              MUST BE 2 CHARS                              
         CLI   5(R2),2                                                          
         BH    ERROR               OR LESS                                      
         OC    8(2,R2),=C'  '                                                   
         EJECT                                                                  
*                                                                               
* CHECK IF GROUP EXISTS                                                         
*                                                                               
         LA    R3,121                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'07'                                                        
         MVC   KEY+23(2),RCONKREP                                               
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   ERROR               GROUP DOESN'T EXIST                          
*                                                                               
* CHECK IF NEW X'9C' KEY EXISTS                                                 
*                                                                               
         LA    R3,390              NEW KEY EXISTS                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'9C'                                                        
         MVC   KEY+2(2),RCONKREP                                                
         MVC   KEY+4(2),RCONKOFF                                                
         MVC   KEY+6(2),8(R2)                                                   
         MVC   KEY+8(5),RCONKSTA                                                
         MVC   KEY+13(4),RCONKADV                                               
         MVC   KEY+17(4),RCONKAGY                                               
         MVC   KEY+21(2),RCONKAOF                                               
         MVC   KEY+23(4),RCONKCON                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED KEY, TOO                   
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    ERROR                                                            
*                                                                               
* CHECK IF NEW X'0C' KEY EXISTS                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RCONKEY),RCONREC                                           
         MVC   KEY+4(2),8(R2)                                                   
         OI    DMINBTS,X'08'       PASS BACK DELETED KEY, TOO                   
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    ERROR                                                            
         EJECT                                                                  
*                                                                               
* CREATE NEW X'0C' KEY                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RCONKEY),RCONREC                                           
         MVC   KEY+4(2),8(R2)                                                   
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VADD                                                             
*                                                                               
* CREATE NEW X'9C' KEY                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'9C'                                                        
         MVC   KEY+2(2),RCONKREP                                                
         MVC   KEY+4(2),RCONKOFF                                                
         MVC   KEY+6(2),8(R2)                                                   
         MVC   KEY+8(5),RCONKSTA                                                
         MVC   KEY+13(4),RCONKADV                                               
         MVC   KEY+17(4),RCONKAGY                                               
         MVC   KEY+21(2),RCONKAOF                                               
         MVC   KEY+23(4),RCONKCON                                               
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VADD                                                             
         EJECT                                                                  
*                                                                               
* UPDATE RECORD                                                                 
*                                                                               
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         MVC   WORK(2),RCONKGRP    SAVE OFF GROUP/SUBGROUP                      
         MVC   RCONKGRP,8(R2)      NEW GROUP/SUBGROUP                           
*                                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         EJECT                                                                  
*                                                                               
* DELETE OLD X'0C' KEY                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RCONKEY),RCONREC                                           
         MVC   KEY+4(2),WORK                                                    
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                ERROR IF NO OLD KEY                          
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
*                                                                               
* DELETE OLD X'9C' KEY                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'9C'                                                        
         MVC   KEY+2(2),RCONKREP                                                
         MVC   KEY+4(2),RCONKOFF                                                
         MVC   KEY+6(2),WORK                                                    
         MVC   KEY+8(5),RCONKSTA                                                
         MVC   KEY+13(4),RCONKADV                                               
         MVC   KEY+17(4),RCONKAGY                                               
         MVC   KEY+21(2),RCONKAOF                                               
         MVC   KEY+23(4),RCONKCON                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                ERROR IF NO OLD KEY                          
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,14     CONTRACT CHANGED                             
*                                                                               
         B     EXXMOD                                                           
         DROP  R7                                                               
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005RECNT2D   04/14/05'                                      
         END                                                                    
