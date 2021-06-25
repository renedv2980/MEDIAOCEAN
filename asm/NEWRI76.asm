*          DATA SET NEWRI76    AT LEVEL 004 AS OF 04/13/04                      
*PHASE T32076A                                                                  
         TITLE 'T32076-KR TAPE'                                                 
T32076   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEKR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)       RA=2ND BASE REGISTER                           
         LA    RA,1(RA)                                                         
         USING T32076,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
*                                                                               
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    VK                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************                                           
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
VK       DS    0H                                                               
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
         MVI   NBQINIT,0                                                        
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         L     RF,ACLISTSV                                                      
         LA    RF,880(RF)                                                       
         MOVE  ((RF),880),CLIST2                                                
         DROP  R3                                                               
*                                                                               
*                                                                               
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
*                                                                               
         LA    R2,SPLSDTH          START                                        
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLEDTH          END                                          
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLBILH          BILLING MONTH                                
         NETGO NVGETFLD,DMCB                                                    
         GOTO1 DATVAL,DMCB,(2,FLD),DUB                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)    YMD BINARY                       
         MVC   BILLMON(1),FULL                SET BILLING YEAR/MON              
         MVC   BILLMON+1(1),FULL+1                                              
         EDIT  (B1,FULL),(2,BDAT)                                               
         EDIT  (B1,FULL+1),(2,BDAT+2)                                           
         CLI   BDAT+2,X'40'        CHANGE BLANKS TO 0                           
         BH    *+8                                                              
         MVI   BDAT+2,C'0'                                                      
*                                                                               
         MVI   TAPEOPT,0                                                        
         LA    R2,SPLTAPEH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    VKEXIT                                                           
         MVC   TAPEOPT,FLD                                                      
         CLI   FLD,C'Y'            PRODUCE TAPE                                 
         BE    *+8                                                              
         CLI   FLD,C'T'            TAPE WITHOUT EDICT                           
         BNE   VK20                                                             
         CLC   =C'OV',CONWHEN                                                   
         BE   VKEXIT                                                            
         MVI   ERROR,INVALID                                                    
         CLC   =C'ON',CONWHEN                                                   
         BE   VKEXIT                                                            
VK20     CLI   FLD,C'N'                                                         
         BE    VKEXIT                                                           
         B     TRAPERR                                                          
*                                                                               
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
INVERR   MVI   ERROR,INVALID                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                                                                               
*                                                                               
RELO     DS    F                                                                
ACLISTSV DS    F                                                                
TAPEOPT  DS    CL1                                                              
BILLMON  DS    CL2                 REQUESTED BILLING YEAR/MONTH                 
BDAT     DS    CL4                 REQUESTED BILLING YYMM                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
         SPACE                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID2D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
BILHD    DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEWRI76   04/13/04'                                      
         END                                                                    
