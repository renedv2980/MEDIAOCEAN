*          DATA SET NEWRIB6    AT LEVEL 009 AS OF 03/14/18                      
*PHASE T320B6A                                                                  
         TITLE 'T320B6 - GP WEEKLY GOALS/ACT/EST - EDIT'                        
T320B6   CSECT                                                                  
         SPACE 3                                                                
*                                                                               
*  BASED ON P3 REPORT AND USES P3 SCREEN                                        
*  BASED ON P3 REPORT AND USES P3 SCREEN                                        
*  BASED ON P3 REPORT AND USES P3 SCREEN                                        
*                                                                               
*              ORGANIZATION OF WORKING STORAGE                                  
*                                                                               
*              ANETWS1 IS A(STORAGE USED BY THIS PROGRAM)                       
*                                                                               
*              STORAGE IS ARRANGED AS FOLLOWS:                                  
*                                                                               
*              NET DEMO BLOCK      516 BYTES                                    
*              DBLOCK              256 BYTES                                    
*              NETGOAL BLOCK       100 BYTES                                    
*              GENERAL STORAGE                                                  
*              PRODUCT LIST       3200 BYTES                                    
*                                                                               
*              REGISTER USAGE                                                   
*                                                                               
*              COMMON DSECTS       RC,R9                                        
*              TWA                 RA                                           
*              SPOOL AREAS         R8                                           
*              STORAGE ABOVE       R7                                           
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**NGP***                                                       
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T320B6+4096,R6                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING NDDEMBLK,R7                                                      
         MVC   NBACLI,ANETWS3      PUT CLI REC HERE                             
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         EJECT                                                                  
*              EDITING REQUEST                                                  
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
         SPACE 1                                                                
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         SPACE 1                                                                
*                                  CLIENT VALIDATION                            
         LA    R2,SPLCLIH                                                       
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         SPACE 1                                                                
*                                  PRODUCT VALIDATION                           
         LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
         SPACE 1                                                                
         MVI   FTERMFLG,1          OTHER FIELDS OPTIONAL                        
         SPACE 1                                                                
*                                  ESTIMATE VALIDATION                          
         LA    R2,SPLESTH                                                       
         NETGO NVEST,DMCB,SPLESTN,NDDEMBLK                                      
         OI    SPLESTNH+6,X'80'                                                 
         NETGO NVDELHOM,DMCB,NDDEMOS                                            
         SPACE 1                                                                
*                                  NETWORK VALIDATION                           
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB,SAVMKT                                             
         SPACE 1                                                                
*                                  DAYPART VALIDATION                           
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN                                               
         OI    SPLDPTNH+6,X'80'                                                 
         MVC   DPFILT,NBSELDP                                                   
         SPACE 1                                                                
*                                  PACKAGE VALIDATION                           
         LA    R2,SPLPAKH                                                       
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
         OI    SPLPAKNH+6,X'80'                                                 
         SPACE 1                                                                
*                                  START DATE                                   
         TM    WHEN,X'20'          IF ITS SOON                                  
         BNO   *+8                                                              
         MVI   FTERMFLG,0          START/END REQUIRED                           
         LA    R2,SPLRSTRH                                                      
         NETGO NVSTRDAT,DMCB                                                    
         SPACE 1                                                                
*                                  DEFAULT END IS START + 6 DAYS                
         LA    R2,SPLRENDH                                                      
         B     ED2                                                              
         CLI   5(R2),0                                                          
         BNE   ED2                                                              
         MVI   5(R2),8                                                          
         GOTO1 ADDAY,DMCB,USERQSTR,WORK,6                                       
         GOTO1 DATCON,DMCB,(0,WORK),(8,8(R2))                                   
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
ED2      NETGO NVENDDAT,DMCB                                                    
         SPACE 1                                                                
         MVI   FTERMFLG,1          RESET OPTIONAL                               
         LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VALIOPT                                                       
         SPACE 1                                                                
         LA    R2,SPLTITLH         USER TITLE                                   
         NETGO NVTITLE,DMCB                                                     
         SPACE 1                                                                
         LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
         SPACE 1                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
         SPACE 1                                                                
XMOD     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 3                                                                
VALIOPT  NTR1                                                                   
         MVI   OPTIONS,0                                                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    OPTERR                                                           
         SPACE 1                                                                
VALIOPT2 CLC   =C'TARG2',12(R3)                                                 
         BNE   VALIOPT3                                                         
         OI    OPTIONS,2                                                        
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPT3 CLC   =C'ASACT',12(R3)                                                 
         BNE   OPTERR                                                           
         OI    OPTIONS,1                                                        
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPTX LA    R3,32(R3)                                                        
         BCT   R4,VALIOPT2                                                      
         B     XIT                                                              
         SPACE 1                                                                
OPTERR   MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF9D                                                       
*                                  NEGENINCLS                                   
*                                  NETDEMOD                                     
*                                  DEDBLOCK                                     
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NENETGOALD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              WORKING STORAGE                                                  
         SPACE 3                                                                
SAVMKT   DS    XL2                 PASSED TO PRINT PHASE                        
DPFILT   DS    CL1                                                              
OPTIONS  DS    CL1                                                              
ACLTREC  DS    A                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009NEWRIB6   03/14/18'                                      
         END                                                                    
