*          DATA SET NEMEDCD    AT LEVEL 016 AS OF 06/18/09                      
*PHASE T31ECDA                                                                  
*                                                                               
         TITLE 'T31ECD - EDIT FOR FILEFIX        '                              
*****************************************************************               
* T31ECD -                                                                      
*                                                                               
*                                                                               
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
T31ECD   CSECT                                                                  
         NMOD1 0,**UNFX**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         MVI   FTERMFLG,1          OPTIONAL                                     
         LA    R2,UVLCLIH          CLIENT                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         CLI   TWAOFFC,C'*'        DDS                                          
         BNE   EDTINV                                                           
         NETGO NVCLIALL,DMCB,UVLCLIN                                            
*                                                                               
         LA    R2,UVLESTH                                                       
         NETGO NVESTALL,DMCB                                                    
*                                                                               
         LA    R2,UVLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,UVLPAKH                                                       
         NETGO NVPAK,DMCB                                                       
*                                                                               
         LA    R2,UVLSTRH                                                       
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,UVLENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,0                                                       
         LA    R2,UVLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BNZ   *+8                                                              
         MVI   UVLTST,C'Y'         DEFAULT TO TEST RUN                          
*                                                                               
         MVI   FTERMFLG,1                                                       
         LA    R2,UVLPRTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BNZ   EDIT20                                                           
         MVI   UVLPRT,C'N'         DEFAULT TO NO PRINT                          
*                                                                               
EDIT20   MVI   FTERMFLG,0                                                       
         MVI   ERROR,INVALID                                                    
         LA    R2,UVLTYPH          S=STATION,P=POST,B=BOTH                      
         NETGO NVGETFLD,DMCB                                                    
         CLI   UVLTYP,C'S'                                                      
         BE    EDITXX                                                           
         CLI   UVLTYP,C'P'                                                      
         BE    EDITXX                                                           
         CLI   UVLTYP,C'T'                                                      
         BE    EDITXX                                                           
         CLI   UVLTYP,C'B'                                                      
         BNE   EDTINV                                                           
*                                                                               
*                                                                               
EDITXX   LA    R2,UVLCLIH                                                       
         B     XMOD                                                             
*                                                                               
XMOD     XIT1                                                                   
         SPACE 2                                                                
*                                                                               
EDTINV   MVI   ERROR,INVALID                                                    
         GOTO1 TRAPERR,DMCB                                                     
*                                                                               
TRAPERR  EQU   ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
* NETINCLS                                                                      
* DDCOMFACSD                                                                    
* NEGETNUND                                                                     
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEMEDFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDCD                                                       
         PRINT ON                                                               
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016NEMEDCD   06/18/09'                                      
         END                                                                    
