*          DATA SET NEMED34    AT LEVEL 004 AS OF 08/10/00                      
*PHASE T31E34A                                                                  
         TITLE 'T31E34-EDIT FOR YNR INTERFACE TAPE'                             
T31E34   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE34**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          R7-ANETWS2/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R6,ANETWS3                                                       
         USING NDDEMBLK,R6         R6-ANETWS3/NDDEMBLK,DEDBLOCK                 
         ST    R6,NBADEM                                                        
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
*                                                                               
*                                  ANETWS4 FOR 3000  IS FREE                    
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMEDPX (T31EPZ) NETWORK INTERFACE TAPE                    *          
*                                                                     *         
*  COMMENTS: WRITES A REPORT/TAPE                                     *         
*                                                                     *         
*  CALLS TO: NETIO                                                              
*                                                                     *         
*  GLOBAL: R7-MYWORKD (ANETWS2+500)                                   *         
*                                                                     *         
***********************                                               *         
*  LOGIC: READS UNIT RECS. SETS UP ONE REQUEST REC(TAPED1)            *         
*         AND SCHEDULE RECS(TAPED), PASSING THESE TO SORTER.          *         
*                                                                     *         
*         AT REQLAST, GETS RECS BACK FROM SORTER WRITING THEM         *         
*         TO TAPE AND PRINT LINE, CREATING A SUMMARY REC(TAPED3)      *         
*         AT BREAKS OF CLT/PRD/EST/NTWK/DAYPT/SPTLN OR WEEKS.         *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************                                           
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
VK       DS    0H                                                               
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
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
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
VK10     LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
*        MVC   DEMSV,NDDEMOS       SAVE TARGET DEMO                             
         CLI   NDDEMOS,X'00'          TEST IF ANY DEMOS                         
         BNE   VK12                   IF NOT/MEANS RANGE                        
         L     R3,NBAIO               SO GET THEM                               
         USING ESTHDR,R3                                                        
*        MVC   DEMSV,EDEMLST      NO DEMO OVERRIDE FOR NOW                      
         MVC   NDDEMOS,EDEMLST                                                  
VK12     OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB                                                       
*                                                                               
         LA    R2,SPLPKGH                                                       
         NETGO NVPAKLOK,DMCB,SPLPKGN                                            
         OI    SPLPKGNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSDTH                                                       
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLEDTH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLDEMH                                                       
         CLI   8(R2),C'E'                                                       
         BE    VK20                                                             
         CLI   8(R2),C'A'                                                       
         BE    VK20                                                             
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VK20     DS    0H                                                               
         LA    R2,SPLDOLH         DOLLAR OPTION                                 
         NETGO NVGETFLD,DMCB        ACT,ASS,B(OTH)                              
         BZ    VK30                                                             
         CLI   SPLDOL,C'B'                                                      
         BNE   VK22                                                             
         MVI   DOLLARS,3                                                        
         B     VK30                                                             
VK22     CLC   SPLDOL(3),=C'ACT'                                                
         BNE   VK24                                                             
         MVI   DOLLARS,1                                                        
         B     VK30                                                             
VK24     CLC   SPLDOL(3),=C'ASS'                                                
         BNE   INVERR                                                           
         MVI   DOLLARS,2                                                        
*                                                                               
VK30     DS    0H                                                               
*                                                                               
VK32     DS    0H                                                               
         LA    R2,SPLTAPEH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    VKEXIT                                                           
         CLI   8(R2),C'Y'                                                       
         BE    VKEXIT                                                           
         CLI   8(R2),C'N'                                                       
         BE    VKEXIT                                                           
         B     TRAPERR                                                          
*                                                                               
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
         SPACE 2                                                                
         LTORG                                                                  
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                                                                               
RELO     DS    F                                                                
ACLISTSV DS    F                                                                
NUMPER   DS    F                                                                
APERLIST DS    F                                                                
DOLLARS  DS    CL1                                                              
INTGFLG DS     CL1                 INTEGRATION OPTION                           
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
         SPACE                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDED                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEMED34   08/10/00'                                      
         END                                                                    
