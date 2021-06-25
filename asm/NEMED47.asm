*          DATA SET NEMED47    AT LEVEL 025 AS OF 05/01/02                      
*PHASE T31E47A                                                                  
         TITLE 'T31E47 - EDIT FOR PERFORM'                                      
T31E47   CSECT                                                                  
*                                                                               
******************************************************************              
*  THIS IS A NEW VERSION OF THE PERFORMANCE REPORT WHICH WILL                   
*  USE THE NEW GETGOAL MODULE. THE OLD VERSION IS SAVED AS NEMED47S.            
*                                                                               
* THIS ORGANIZATION OF W/S MUST ALSO BE REFLECTED IN NEMED67                    
*                                                                               
*   ORGANIZATION OF WORKING STORAGE                                             
*                                                                               
*  ANETWS1 ->  NET DEMO BLOCK                                                   
*                                                                               
*              DBLOCK                                                           
*                                                                               
*              ARGS FROM EDIT TO PRINT MODULE                                   
*                     DAYPART FILTER        CL1                                 
*                     OPTIONS               CL8                                 
*                     PACKED MARKET         H                                   
*                                                                               
*                                                                               
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**PEED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*        INITIALIZE                                                             
         SPACE 3                                                                
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R7,ANETWS1          SET A(NET DEMO BLOCK)                        
         ST    R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
         LA    R3,DBLOCK           SET UP DBLOCK                                
         USING DBLOCK,R3                                                        
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         LA    R2,SPLCLIH          CLIENT. REQUIRED.                            
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'    TRANSMIT PRODUCT NAME.                       
*                                                                               
         LA    R2,SPLESTH          ESTIMATE. REQUIRED.                          
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         OI    SPLESTNH+6,X'80'    XMIT ESTIMATE NAME                           
         NETGO NVDELHOM,DMCB,NDDEMOS   REMOVE HOMES REQUESTS                    
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB,SAVMKT    GET MARKET NUMBER                        
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN   DAYPART. OPTIONAL.                          
         OI    SPLDPTNH+6,X'80'    TRANSMIT DAYPART NAME                        
         MVC   DPFILT,NBSELDP                                                   
*                                                                               
         LA    R2,SPLTYPEH                                                      
         GOTO1 ANY                                                              
         CLI   SPLTYPE,C'B'                                                     
         BE    EDB                                                              
**       BNE   EDOPT2                                                           
**       CLC   SPLPRO(3),=C'POL'                                                
**       BE    EDB                                                              
**       B     EDOPTX                                                           
EDOPT2   CLI   SPLTYPE,C'W'                                                     
         BE    EDB                                                              
         CLI   SPLTYPE,C'N'                                                     
         BE    EDB                                                              
EDOPTX   MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 2                                                                
EDB      LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
ED4      LA    R2,SPLOPTH                                                       
         BAS   RE,VALIOPT                                                       
         LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
         SPACE 2                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 3                                                                
VALIOPT  NTR1                                                                   
         MVC   OPTIONS,=8C'N'      PRESET TO N                                  
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R4,8(R2)                                                         
         SPACE 2                                                                
VALIOPT2 CLI   0(R4),X'F1'                                                      
         BL    VALIOPT4                                                         
         CLI   0(R4),X'F8'                                                      
         BH    VALIOPT4                                                         
         IC    R1,0(R4)                                                         
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         BCTR  R1,0                                                             
         LA    R1,OPTIONS(R1)                                                   
         MVI   0(R1),C'Y'                                                       
         LA    R4,1(R4)                                                         
         BCT   R3,VALIOPT2                                                      
         B     XIT                                                              
         SPACE 2                                                                
VALIOPT4 MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE7D                                                       
*                                                                               
       ++INCLUDE NETDEMOD                                                       
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
***** ARGS TO PRINT MODULE                                                      
DPFILT   DS    CL1                 DAYPART FILTER                               
OPTIONS  DS    CL8                 OPTION LIST                                  
SAVMKT   DS    H                   MARKET OF NETWORK SPECIFIED                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NEMED47   05/01/02'                                      
         END                                                                    
