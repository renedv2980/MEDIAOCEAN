*          DATA SET NEMED45    AT LEVEL 007 AS OF 06/06/00                      
*PHASE T31E45A,+0                                                               
         TITLE 'T31E45 - EDIT FOR EVALUATION SUMMARY'                           
*                                                                               
*  PASSES NETDEMO BLOCK AND DBLOCK TO EDIT ROUTINE IN W/S AREA 2                
*                                                                               
T31E45   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SUED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*              INITIALIZE NETBLOCK                                              
         SPACE 3                                                                
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         L     R7,ANETWS2          USE W/S AREA 2 FOR NET DEMO BLOCK            
         ST    R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
         MVI   FTERMFLG,0          SAYS FOLLOWING FIELDS ARE REQUIRED           
         LA    R2,SPLCLIH          CLIENT.REQUIRED.                             
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIN                                
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'    TRANSMIT PRODUCT NAME                        
*                                                                               
         LA    R2,SPLESTH          ESTIMATE. REQUIRED.                          
         NETGO NVEST,DMCB,SPLESTN,NDDEMBLK   GET DEMOS FROM HEADER.             
         NETGO NVDELHOM,DMCB,NDDEMOS        REMOVE HOMES REQUESTS.              
         OI    SPLESTNH+6,X'80'    TRANSMIT ESTIMATE NAME                       
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN                                               
         OI    SPLDPTNH+6,X'80'    TRANSMIT DAYPART NAME                        
*                                                                               
         LA    R2,SPLPAKH                                                       
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
DEMFLD   LA    R2,SPLDEMSH                                                      
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    ED1                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'FIRST'                                                
         BE    ED2                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SECOND'                                               
         BE    ED3                                                              
*                                                                               
ED1      NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK   VALIDATE ENTERED DEMOS              
         XC    NDDEMOS+21(21),NDDEMOS+21    DONT ALLOW MORE THAN 7              
         B     ED4                                                              
         SPACE 1                                                                
ED2      DS    0H                  OPTION TO TAKE FIRST 7 DEMOS                 
         OC    NDDEMOS+21(3),NDDEMOS+21 ZERO IF 7 OR LESS DEMOS                 
         BZ    ED4                                                              
         XC    NDDEMOS+21(21),NDDEMOS+21                                        
         B     ED4                                                              
*                                                                               
ED3      DS    0H                  OPTION TO TAKE SECOND 6                      
         OC    NDDEMOS+21(3),NDDEMOS+21  ZERO IF 7 OR LESS DEMOS                
         BZ    ED4                                                              
         MVC   NDDEMOS(21),NDDEMOS+21                                           
         XC    NDDEMOS+21(21),NDDEMOS+21                                        
         SPACE 2                                                                
ED4      LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
         SPACE 2                                                                
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE5D                                                       
*                                                                               
         DSECT                     INFO PASSED TO EDIT                          
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NEMED45   06/06/00'                                      
         END                                                                    
