*          DATA SET RECNT16    AT LEVEL 019 AS OF 04/23/93                      
*PHASE T80216A,+0                                                               
         TITLE 'T80216 - COMBO BUY COMMENT DISP/EDIT'                           
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT16 (T80216) --- COMBO BUY COMMENT DISP/EDIT         *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 23APR93 (SKU) --- ORIGINAL ROLLOUT                              *             
*                                                                 *             
*HERE**************************************************************             
*                                                                               
T80216   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80216,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*                                                                               
* CLEAR DISPLAY FIELDS                                                          
         TWAXC BCMCMBSH                                                         
*                                                                               
         B     EXXMOD              EXIT OVERLAY                                 
*                                                                               
         EJECT                                                                  
XIT      XIT1                                                                   
       ++INCLUDE RECNTWRK                                                       
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTE6D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019RECNT16   04/23/93'                                      
         END                                                                    
