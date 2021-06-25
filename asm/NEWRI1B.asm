*          DATA SET NEWRI1B    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T3201BA                                                                  
         TITLE 'T3201B - EDIT FOR P4 (GE NETWORK ALLOCATION)'                   
T3201B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**P4ED**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE W/S AREA 2 FOR ARGS TO PRINT             
         USING EDD,R7                                                           
         ST    R7,NBADEM           1ST PART IS NET DEMO BLOCK                   
         L     R1,ANETWS1          PASS CLIENT RECORD IN W/S AREA 1             
         ST    R1,NBACLI                                                        
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         EJECT                                                                  
*              INITIALIZE                                                       
         SPACE 3                                                                
AE2      MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         CLI   OFFLINE,C'Y'                                                     
         BNE   AE3                 ALLOW FOR DDS REQUEST                        
         CLC   SPLCLI(3),=C'***'                                                
         BNE   AE3                                                              
         XC    NBSELAGY,NBSELAGY                                                
         XC    NBEFFAGY,NBEFFAGY                                                
         MVC   SPLCLI(3),=C'ALL'                                                
         EJECT                                                                  
*              EDIT CLI/PRD/EST/NET/DPT/PACKAGE                                 
         SPACE 3                                                                
AE3      MVI   FTERMFLG,0          (REQUIRED FIELDS)                            
         SPACE 1                                                                
*                                  CLIENT VALIDATION                            
         LA    R2,SPLCLIH                                                       
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
         SPACE 1                                                                
*                                  PRODUCT & PRODUCT GROUP                      
         LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
         SPACE 1                                                                
*                                  ESTIMATE VALIDATION                          
         LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         OI    SPLESTNH+6,X'80'                                                 
         SPACE 1                                                                
         MVI   FTERMFLG,1          (OPTIONAL FIELDS)                            
         SPACE 1                                                                
*                                  NETWORK VALIDATION                           
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
         SPACE 1                                                                
*                                  DAYPART VALIDATION                           
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN                                               
         OI    SPLDPTNH+6,X'80'                                                 
         SPACE 1                                                                
*                                  PACKAGE VALIDATION                           
         LA    R2,SPLPAKH                                                       
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
         OI    SPLPAKNH+6,X'80'                                                 
         EJECT                                                                  
*              EDIT THE SELECTIONS                                              
         SPACE 3                                                                
         LA    R2,SPLLENH          LENGTHS                                      
         BAS   RE,VALLEN                                                        
         SPACE 1                                                                
         LA    R2,SPLRSTRH         RUN START                                    
         NETGO NVSTRDAT,DMCB                                                    
         LA    R2,SPLRENDH             AND END                                  
         ZIC   R3,WHEN             (SAVE 'WHEN' VALUE)                          
         MVI   WHEN,0              (ALLOWS SOON REQUESTS TO GO THROUGH)         
         NETGO NVENDDAT,DMCB                                                    
         STC   R3,WHEN             (AND RESTORE)                                
         SPACE 1                                                                
         LA    R2,SPLDEMH          DEMOS                                        
         MVI   NDNDEMOS,1          (MAX 1)                                      
         NETGO NVDELHOM,DMCB,NDDEMOS                                            
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         SPACE 1                                                                
EDOPT    LA    R2,SPLOPTH          OPTIONS                                      
         BAS   RE,VALIOPT                                                       
         SPACE 1                                                                
         LA    R2,SPLTITLH         TITLE                                        
         NETGO NVTITLE,DMCB                                                     
         SPACE 1                                                                
         LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
         SPACE 1                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
         SPACE 1                                                                
EDERR2   MVC   CONHEAD,WORK                                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2,DMCB                                                      
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 3                                                                
VALIOPT  NTR1                                                                   
         MVI   SPACOPT,1                                                        
         MVI   BOXOPT,C'Y'                                                      
         MVI   LEFTOPT,C'N'                                                     
         MVI   PFBOPT,C'N'                                                      
         MVI   SCHOPT,C'A'                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         SPACE 1                                                                
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    OPTERR                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R3),=C'ACT'                                                 
         BNE   OPT3                                                             
         MVI   SCHOPT,C'A'                                                      
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT3     CLC   12(3,R3),=C'EST'                                                 
         BNE   OPT4                                                             
         MVI   SCHOPT,C'E'                                                      
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT4     CLI   12(R3),C'S'         SPACING                                      
         BNE   OPT6                                                             
         MVC   SPACOPT,11(R3)                                                   
         CLI   SPACOPT,0                                                        
         BE    OPTERR                                                           
         CLI   SPACOPT,3                                                        
         BH    OPTERR                                                           
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT6     CLC   12(3,R3),=C'BOX'     BOX PRINTING OPTION                         
         BNE   OPT8                                                             
         MVC   BOXOPT,22(R3)                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT8     CLC   12(4,R3),=C'LEFT'   LEFT ALIGN OPTION                            
         BNE   OPT10                                                            
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT10    CLC   12(3,R3),=C'PFB'    PFB OPTION                                   
         BNE   OPT12                                                            
         MVI   PFBOPT,C'Y'                                                      
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT12    DS    0H                                                               
         SPACE 1                                                                
OPTERR   MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 1                                                                
OPTNEXT  LA    R3,32(R3)                                                        
         BCT   R4,OPT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EDIT SPOT LENGTH ANALYSIS                             
         SPACE 3                                                                
VALLEN   NTR1                                                                   
         MVI   LENS,15             DEFAULT IS 15,30,60                          
         MVI   LENS+1,30                                                        
         MVI   LENS+2,60                                                        
         MVI   NLENS,3                                                          
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         SPACE 1                                                                
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    LENERR                                                           
         LA    R5,LENS                                                          
         XC    LENS,LENS                                                        
         STC   R4,NLENS            RETURN N'ITEMS                               
         SPACE 1                                                                
VALLEN2  L     R1,4(R3)            THIS SHOULD BE NUMBERIC ITEM                 
         LTR   R1,R1                                                            
         BZ    LENERR                                                           
         CH    R1,=H'240'          NOT MORE THAT 240                            
         BH    LENERR                                                           
         STC   R1,0(R5)                                                         
         LA    R3,32(R3)                                                        
         LA    R5,1(R5)                                                         
         BCT   R4,VALLEN2                                                       
         B     XIT                                                              
         SPACE 1                                                                
LENERR   MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         EJECT                                                                  
*              DSECT FOR EDIT                                                   
         SPACE 3                                                                
EDD      DSECT                     COMMON WITH PRINT                            
*                                  NETDEMOD                                     
*                                  DEDBLOCK                                     
         PRINT OFF                                                              
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
SPACOPT  DS    CL1                                                              
BOXOPT   DS    CL1                                                              
LEFTOPT  DS    CL1                                                              
PFBOPT   DS    CL1                                                              
SCHOPT   DS    CL1                                                              
LENS     DS    XL3                                                              
NLENS    DS    XL1                                                              
         SPACE 1                                                                
*              NEGENINCLS HERE                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              BASE SCREEN DSECT                                                
         SPACE 3                                                                
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
*              OVERLAY SCREEN                                                   
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIFBD                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEWRI1B   05/01/02'                                      
         END                                                                    
