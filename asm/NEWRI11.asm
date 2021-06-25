*          DATA SET NEWRI11    AT LEVEL 034 AS OF 05/01/02                      
*PHASE T32011A                                                                  
         TITLE 'T32011 - SPECIAL REPORT PHASE'                                  
T32011   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPEC**,RR=R2                                                 
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING MYD,R7                                                           
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
         SPACE 1                                                                
RP2      CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,EDITMOD                                                       
         B     XMOD                                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT MODULE                                                      
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   FTERMFLG,0          (REQUIRED FIELDS)                            
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
         SPACE 1                                                                
         LA    R2,SPLRSTRH         RUN START                                    
         NETGO NVSTRDAT,DMCB                                                    
         LA    R2,SPLRENDH             AND END                                  
         NETGO NVENDDAT,DMCB                                                    
         SPACE 1                                                                
         LA    R2,SPLTITLH         TITLE                                        
         NETGO NVTITLE,DMCB                                                     
         SPACE 1                                                                
         LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
         SPACE 2                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
         SPACE 1                                                                
XMOD     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              REPORT MODULE                                                    
         SPACE 3                                                                
REPMOD   NTR1                                                                   
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         MVI   NBDATA,C'U'                                                      
         SPACE 1                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    GOTONE                                                           
         CLI   NBMODE,NBREQLST                                                  
         BE    XIT                                                              
         B     GETUNIT                                                          
         SPACE 1                                                                
GOTONE   L     R2,NBPAYTGR         CHECK ASSIGNED V PAID TIME                   
         L     R3,NBASSIGN                                                      
         BAS   RE,CHECK                                                         
         L     R3,NBACTUAL         CHECK ACTUAL V PAID TIME                     
         BAS   RE,CHECK                                                         
         L     R2,NBPAYIGR         CHECK INTEGRATION V PAID                     
         L     R3,NBINTEG                                                       
         BAS   RE,CHECK                                                         
         B     GETUNIT                                                          
         SPACE 1                                                                
CHECK    NTR1                                                                   
         LTR   R2,R2               MUST BE ACTIVE                               
         BZ    XIT                                                              
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         CR    R2,R3               IF PAID IS GREATER THAN ORDER                
         BH    HIT                                                              
         B     XIT                                                              
         SPACE                                                                  
*                                  GOT A HIT SO REPORT                          
HIT      MVC   P+1(3),NBCLICOD                    CLIENT CODE                   
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,P+5)   DATE                          
         EDIT  (1,NBACTEST),(3,P+11)              ESTIMATE                      
         EDIT  (1,NBACTPAK),(3,P+15)              PACKAGE                       
         MVC   P+19(4),NBACTNET                   NETWORK                       
         MVC   P+24(16),NBPROGNM                  PROGRAM NAME                  
         MVC   P+41(3),NBDAYNAM                   DAY                           
         GOTO1 UNTIME,DMCB,NBTIME,P+45            TIME                          
         EDIT  (1,NBLEN),(3,P+57)                 LENGTH                        
         EDIT  (R3),(12,P+62),2,MINUS=YES         ON ORDER                      
         EDIT  (R2),(12,P+75),2,MINUS=YES         CLEARED                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
DAYLIST  DC    C'M-FMONTUEWEDTHUFRISATSUNM-S'                                   
         EJECT                                                                  
*              HEADLINE ROUTINES, ETC                                           
         SPACE 3                                                                
HOOK     NTR1                              HEAD HOOK                            
         MVC   H1+45(22),=C'DOUBLE CLEARANCE CHECK'                             
         NETGO NVTITOUT,DMCB                                                    
         MVC   H8(132),HEADINGS                                                 
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         MVC   BOXCOLS(132),MYCOLS                                              
         B     XIT                                                              
         EJECT                                                                  
*              SPECS FOR PHASE                                                  
         SPACE 3                                                                
MYSPECS  DS    0F                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,49,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,99,PAGE                                                       
         SSPEC H6,1,C'ESTIMATE'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
*              STORAGE  LTORG ETC                                               
         SPACE 3                                                                
RELO     DS    A                                                                
HEADINGS DC    C' CLI DATE  EST PAK NET  PROGRAM NAME     DAY TIME'             
         DC    C'        LEN  '                                                 
         DC    C'    COST     '                                                 
         DC    C'   CLEARED   '                                                 
         DC    CL100' '                                                         
         SPACE 1                                                                
MYCOLS   DC    C'L   C     C   C   C    C                C   C    '             
         DC    C'       C   CC'                                                 
         DC    C'            C'                                                 
         DC    C'            R'                                                 
         DC    CL100' '                                                         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              NETINCLS AND MODULE W/S                                          
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 1                                                                
MYD      DSECT                                                                  
*                                  WORKING STORAGE                              
DPGOPT   DS    CL1                                                              
*                                                                               
*                                  DEDBLOCK & NETDEMOD                          
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE NEWRIFFD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF1D                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034NEWRI11   05/01/02'                                      
         END                                                                    
