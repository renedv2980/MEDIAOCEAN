*          DATA SET NEMED44    AT LEVEL 021 AS OF 02/25/20                      
*PHASE T31E44B                                                                  
         TITLE 'T31E44 - EDIT FOR NETWORK LOCK'                                 
T31E44   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**VDLD**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
*                                                                               
         LA    R2,SPLMEDH          MEDIA TYPE - OPTIONAL                        
*                                                                               
*        CLI   OFFLINE,C'Y'                                                     
*        BE    *+12                                                             
*        CLI   TWAOFFC,C'*'        DDS ONLY                                     
*        BNE   EDERR                                                            
         CLI   TWAWHEN,X'02'       SOON ?                                       
         BNE   *+12                                                             
         CLI   SPLTESTH+8,C'Y'     TEST RUN = Y                                 
         BNE   EDERR2              SOON ALLOWED ONLY WITH TEST RUN=Y            
*                                                                               
         CLI   SPLMED,X'40'                                                     
         BNH   ED2                                                              
         CLI   SPLMED,C'N'                                                      
         BE    ED2                                                              
         CLI   SPLMED,C'S'                                                      
         BE    ED2                                                              
         CLI   SPLMED,C'C'                                                      
         BE    ED2                                                              
         CLI   SPLMED,C'O'                                                      
         BE    ED2                                                              
         CLI   SPLMED,C'D'                                                      
         BE    ED2                                                              
         CLI   SPLMED,C'V'                                                      
         BE    ED2                                                              
         CLI   SPLMED,C'X'    NETWORK RADIO                                     
         BE    ED2                                                              
         B     EDERR                                                            
*                                                                               
                                                                                
*                                                                               
ED2      LA    R2,SPLSTRTH                                                      
         CLI   8(R2),X'40'                                                      
         BNH   EDERR                                                            
         NETGO NVSTRDAT,DMCB       START DATE. REQUIRED.                        
*                                                                               
*                                                                               
ED4      LA    R2,SPLTESTH         TEST RUN ?                                   
         CLI   8(R2),C'N'                                                       
         BE    XMOD                                                             
         CLI   8(R2),C'Y'                                                       
         BE    XMOD                                                             
         MVI   8(R2),C'Y'          DEFAULT                                      
         B     XMOD                                                             
*                                                                               
EDERR2   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(60),ERRMSG1                                              
         LA    R2,SPLTESTH                                                      
         GOTO1 ERREX2                                                           
         B     XMOD                                                             
*                                                                               
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
*                                                                               
ERRMSG1  DC   CL60'LIVE RUNS CAN ONLY BE REQUESTED USING THE OV OPTION'         
         LTORG                                                                  
*                                                                               
SPLCLTH  DS    CL8                                                              
SPLCLT   DS    CL3                                                              
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE4D                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021NEMED44   02/25/20'                                      
         END                                                                    
