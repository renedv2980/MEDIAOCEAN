*          DATA SET NEMED09    AT LEVEL 013 AS OF 08/10/00                      
*PHASE T31E09A                                                                  
         TITLE 'T31E09 - EDIT GRP FLOW CHART'                                   
T31E09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE09**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS3          R7-WORKING STORAG (ANETWS3+500)              
         LA    R7,500(R7)                                                       
         USING WORKD,R7                                                         
         L     R6,ANETWS4                                                       
         USING NDDEMBLK,R6         R6-NDDEMBLK,DEDBLOCK (ANETWS4)               
         ST    R6,NBADEM                                                        
         ST    R2,RELO                                                          
         L     R2,ANETWS2          ANETWS2+500 GETS CLISTSV(880)                
         LA    R2,500(R2)                                                       
         ST    R2,ACLISTSV                                                      
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMED86 (T31E09) PRE-BUY SCHEDULE                           *         
*                                                                     *         
*  COMMENTS: WRITES A REPORT THAT BREAKS OUT GOAL AND EST GRPS        *         
*            I.E. GRPS FROM GOAL REC VS FROM UNIT REC                 *         
*                                                                     *         
*  CALLS TO: NETIO,NETGOAL                                            *         
*                                                                     *         
*  GLOBAL: R7-MYWORKD (ANETWS3+500)                                   *         
*          R6-NDDEMBLK,DEDBLOCK (ANETWS4)                             *         
*                                                                     *         
***********************                                               *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         B     VK                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       DS    0H                                                               
         SPACE                                                                  
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB                      PICK UP ADDRESS OF NETGOAL          
         ST    RF,ANETGOAL                                                      
         SPACE                                                                  
         GOTO1 NBCALLOV,DMCB,0,X'D9000AE0'                                      
         L     RF,DMCB                      PICK UP ADDRESS OF DEMOCON          
         ST    RF,ADEMOCON                                                      
         SPACE                                                                  
* VALIDATE SCREEN INPUT FIELDS                                                  
*                                                                               
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1          SET OPTIONAL FLAG                            
*                                                                               
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK10                                                             
         CLC   8(3,R2),=C'POL'                                                  
         BE    VK10                                                             
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
VK10     LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         MVC   DEMSV,NDDEMOS       SAVE TARGET DEMO                             
         CLI   NDDEMOS,X'00'          TEST IF ANY DEMOS                         
         BNE   VK12                   IF NOT/MEANS RANGE                        
         L     R3,NBAIO               SO GET THEM                               
         USING ESTHDR,R3                                                        
         MVC   DEMSV,EDEMLST                                                    
         MVC   NDDEMOS,EDEMLST                                                  
VK12     OI    SPLESTNH+6,X'80'                                                 
*                                                                               
VK20     LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
VK25     LA    R2,SPLPAKH                                                       
         NETGO NVPAK,DMCB                                                       
*                                                                               
VK30     DS    0H                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPTALL,DMCB                                                    
*                                                                               
VK40     LA    R2,SPLSDTH                                                       
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
VK50     LA    R2,SPLEDTH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLOPTH                                                       
         CLI   8(R2),C'A'                                                       
         BNE   *+8                                                              
         MVI   DEMOPT,C'A'         SET ACTUAL DEMOS OPTION                      
*                                                                               
         LA    R2,SPLPRTH          PROD SUBTOT OPTION                           
         MVI   PRDTFLG,0                                                        
         CLI   8(R2),0                                                          
         BE    VKEXIT                                                           
         CLI   8(R2),C'N'                                                       
         BE    VKEXIT                                                           
         MVI   PRDTFLG,C'Y'                                                     
         CLI   8(R2),C'Y'                                                       
         BE    VKEXIT                                                           
VERR     MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS3+500                     
BINDMCB  DS    6F                                                               
ANETGOAL DS    V                                                                
ADEMOCON DS    V                                                                
ACLISTSV DS    A                                                                
WORKAD   DS    A                   TEMP WORKING ADDRESS                         
AWKTOTBL DS    A                                                                
*                                                                               
RELO     DS    F                                                                
NUMPER   DS    F                                                                
AP1      DS    F                                                                
AH10     DS    F                                                                
ABOXCOLS DS    F                                                                
GTOTL    DS    F                                                                
ETOTL    DS    F                                                                
TBL      DS    F                                                                
TBL2     DS    F                                                                
PERTYPE  DS    CL3                                                              
DEMSV    DS    CL3                                                              
PRDSV    DS    CL3                                                              
PRDNOSV  DS    CL1                                                              
SPTLNSV  DS    XL1                                                              
PERLIST  DS    CL60                                                             
         DS    CL1                                                              
*                                                                               
FRST     DS    CL1                                                              
BOXSET   DS    CL1                                                              
DEMOPT   DS    CL1                                                              
PRDTFLG  DS    CL1                                                              
         DS    CL14                SPARE                                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
       ++INCLUDE NEMEDFFD                                                       
         ORG CONTAGH                                                            
       ++INCLUDE NEMEDF9D                                                       
         EJECT                                                                  
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013NEMED09   08/10/00'                                      
         END                                                                    
