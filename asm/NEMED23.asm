*          DATA SET NEMED23    AT LEVEL 006 AS OF 08/10/00                      
*PHASE T31E23A                                                                  
         TITLE 'T31E23 - PACKAGE LISTING'                                       
T31E23   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTLI**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         L     R6,NBAIO                                                         
         USING NPRECD,R6           R6 PTS TO PACKAGE RECORD                     
*                                                                               
         EJECT                                                                  
*                                                                               
         MVI   NBDATA,C'P'         ONLY READ PACKAGES                           
*                                                                               
GETPACK  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    ALLDONE                                                          
         CLI   NBMODE,NBPROCPK                                                  
         BE    GOTPACK                                                          
         B     GETPACK                                                          
*                                                                               
GOTPACK  MVC   P+2(4),NPKNET                                                    
         EDIT  (1,NPKEST),(3,P+12),FILL=0                                       
         EDIT  (1,NPKPACK),(3,P+21),FILL=0                                      
         MVC   P+29(16),NPAKNAME                                                
         MVC   P+47(8),NBDPNAM                                                  
         EDIT  (4,NPAKCOST),(9,P+56)                                            
         LA    R2,P+68                                                          
         TM    NPAKSTAT,X'80'                                                   
         BNO   LI4                                                              
         MVC   0(6,R2),=C'FROZEN'                                               
         LA    R2,7(R2)                                                         
*                                                                               
LI4      TM    NPAKSTAT,X'20'                                                   
         BNO   LI6                                                              
         MVC   0(6,R2),=C'LOCKED'                                               
*                                                                               
LI6      EDIT  (2,NPAKFEED),(6,P+79),2,ZERO=BLANK                               
         EDIT  (2,NPAKUNIV),(6,P+88),2,ZERO=BLANK                               
         OC    NPAKUNCD,NPAKUNCD                                                
         BZ    LI8                                                              
         UNPK  P+88(5),NPAKUNCD(3)                                              
         MVC   P+92(2),SPACES                                                   
*                                                                               
LI8      EDIT  (4,NPAKINT),(9,P+95),2,ZERO=BLANK                                
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     GETPACK                                                          
*                                                                               
*                                                                               
ALLDONE  XIT1                                                                   
*                                                                               
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         CLI   NBSELESE,0                                                       
         BE    HOOK1                                                            
         MVC   H6+10(7),SPLEST                                                  
         MVI   H6+17,C' '                                                       
         MVC   H6+18(24),SPLESTN                                                
         OC    H6+10(32),SPACES                                                 
         GOTO1 SQUASHER,DMCB,H6+10,32                                           
         SPACE 2                                                                
HOOK1    MVC   H1+43(15),=C'PACKAGE LISTING'                                    
         MVC   H2+43(15),=15C'-'                                                
         MVC   H5+82(8),SPLDPTN                                                 
         XIT1                                                                   
         EJECT                                                                  
****           LTORG                                                            
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF3D                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEMED23   08/10/00'                                      
         END                                                                    
