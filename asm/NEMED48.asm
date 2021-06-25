*          DATA SET NEMED48    AT LEVEL 004 AS OF 04/14/04                      
*          DATA SET NEMED48    AT LEVEL 036 AS OF 04/29/94                      
*PHASE T31E48A,+0                                                               
*                                                                               
         TITLE 'T31E48 - EDIT FOR LATEST EST DEMO UPDATE/REPORT'                
**********************************************************************          
* NEMED07(T31E407) - THIS EDITS LATEST ESTIMATED DEMO REPORT          *         
*                                                                               
* INPUTS - PARAMETER 1 - LOCATION OF THE GEND SPOOL DSECT. CONTAINS  *          
*                           MANY USEFUL ADDRESSES, DATA              *          
*                                                                               
* OUTPUTS - NETBLOCK - THE BLOCK USED TO READ THE NETWORK FILE.      *          
*                      MANY FIELDS FILLED IN BY NETIO.               *          
*                                                                               
*                                                                               
*  CALLS TO -                                                        *          
*   NVVALID - VALIDATION ROUTINES.                                   *          
**********************************************************************          
*                                                                               
         PRINT NOGEN                                                            
T31E48   CSECT                                                                  
         NMOD1 0,**NE48**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         USING MYWORKD,R7                                                       
         ST    R2,RELO                                                          
         L     R4,ANETWS4                                                       
         USING NDDEMBLK,R4                                                      
         ST    R4,NBADEM                                                        
         EJECT                                                                  
*************  INITIALIZE NETBLOCK*************                                 
*                             ASSUMES NETBLOCK IS ALREADY INITIALIZED           
*                             DONE BY CALL TO NVAGY OR NVAGYOUT                 
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NEEDED IF ALREADY VALIDATED BITS           
*                                    NOT USED                                   
*                                    TO PASS TO PRINT MODULE                    
*                                                                               
         LA    R2,SPLCLIH          CLIENT                                       
         NETGO NVCLI,DMCB,SPLCLIN      FILL IN NAME                             
         OI    SPLCLINH+6,X'80'           TRANSMIT NAME                         
         CLI   OFFLINE,C'Y'                                                     
         BNE   VALPRD                                                           
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         MOVE  (CLISTSV,880),CLIST      SAVE CLIST                              
         MOVE  (CLISTSV+880,140),CLIST2                                         
         MVC   CLTNMSV,CNAME                                                    
         DROP  R3                                                               
*                                                                               
VALPRD   LA    R2,SPLPROH          PRODUCT                                      
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         NETGO NVPRDALL,DMCB,SPLPRON      AND FILL IN NAME                      
         CLC   =C'ALL',SPLCLI      IF CLI=ALL SKIP PRDNM FOUT                   
         BE    *+8                 SINCE YOU GET GARBAGE                        
         OI    SPLPRONH+6,X'80'           TRANSMIT PRODUCT NAME                 
*                                                                               
         LA    R2,SPLESTH          ESTIMATE                                     
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK  AND FILL IN NAME                 
         OI    SPLESTNH+6,X'80'             TRANSMIT NAME                       
         CLC   =3X'00',NDDEMOS                                                  
         BNE   EDT5                                                             
         L     R3,NBAIO                                                         
         USING ESTHDR,R3                                                        
         MVC   NDDEMOS,EDEMLST                                                  
         MVI   NDNDEMOS,1                                                       
*                                                                               
EDT5     LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB,SPLNETN      AND FILL IN NAME                      
         BZ    *+8                                                              
         OI    SPLNETNH+6,X'80'           TRANSMIT NAME                         
*                                                                               
         LA    R2,SPLPAKH          PACKAGE                                      
         CLC   =C'ALL',SPLPAK                                                   
         BE    EDT10                                                            
         MVI   FTERMFLG,1               SET OPTIONAL FLAG                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT10                                                            
         NETGO NVPAK,DMCB,SPLPAKN     ELSE VALIDATE/FILL IN SPLPAKN             
         OI    SPLPAKNH+6,X'80'       TRANSMIT PAKN                             
*                                                                               
EDT10    DS    0H        READ PROGRAM REC TO VALIDATE NAME                      
         LA    R2,SPLPRGMH                                                      
         MVI   FTERMFLG,1               SET OPTIONAL FLAG                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         MVC   NBSELPRG,SPACES     NBSELPRG NEEDS SPACES                        
         ZIC   R1,5(R2)            LENGTH OF INPUT TO R1                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     EDT20                                                            
         MVC   NBSELPRG(0),SPLPRGM     SO DO EX MOVE                            
*  PUT IN THE LOGIC TO ACTUALLY READ THE PROGRAM REC                            
*                                                                               
*                                                                               
EDT20    DS    0H                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE                                   
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB       END DATE                                     
         EJECT                                                                  
         SPACE                                                                  
*                                  TEST RUN CHECK                               
         LA    R2,SPLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         CLI   SPLTST,C'Y'         TEST RUN                                     
         BE    EDT30                                                            
         CLI   SPLTST,C'N'         MARK FILE                                    
         BNE   EDTERR                                                           
         CLC   =C'SOON',CONWHEN    SOON CANNOT MARK FILE                        
         BNE   EDT30                                                            
EDTERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
EDT30    DS    0H                  EST/ACT OPTION                               
         LA    R2,SPLEATH                                                       
         CLI   SPLEAT,C'E'         PROGRAM REC TO EST                           
         BE    EDT40                                                            
         CLI   SPLEAT,C'A'         PROGRAM REC TO ACT                           
         BE    EDT40                                                            
         CLI   SPLEAT,C'B'         PRO TO ACT EVEN IF ACTUALS ARE IN            
         BE    EDT40                                                            
         CLI   SPLEAT,C'C'         PRO TO EST EVEN IF ACTUALS ARE IN            
         BE    EDT40                                                            
         CLI   SPLEAT,C'0'         RE-LOOKUP ACTUAL DEMOS                       
         BE    EDT40                                                            
         CLI   SPLEAT,C'1'         TAKE NTI CODE OFF PROGRAM RECORD             
         BE    EDT40                                                            
ED30ERR  MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 3                                                                
EDT40    LA    R2,SPLMKDH        MAKE GOODS GET LATEST EST DEMO OPTION          
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT45                                                            
         CLI   FLD,C'Y'                                                         
         BNE   EDT45                                                            
         MVI   MKGDMDGD,C'Y'                                                    
         SPACE 3                                                                
EDT45    DS    0H                                                               
         LA    R2,SPLOVRH         REPLACE OVERRIDES OPTION                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT47                                                            
         CLI   FLD,C'Y'                                                         
         BNE   EDITXX                                                           
         MVI   OVERIDE,C'Y'                                                     
         SPACE 3                                                                
EDT47    DS    0H              IF EST=ALL, GET LOWEST EST IN STRT/END           
***      CLI   NBSELEST,0                                                       
***      BNE   EDITXX                                                           
*****    BAS   RE,LTSTEST **** FORGET THIS FOR NOW                              
         LA    R2,SPLPNMH         REPLACE OVERRIDES OPTION                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDITXX                                                           
         CLI   FLD,C'Y'                                                         
         BNE   EDITXX                                                           
         MVI   PROGNAM,C'Y'                                                     
         SPACE 3                                                                
EDITXX   LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         B     XMOD                                                             
         SPACE                                                                  
*                                                                               
XMOD     XIT1                                                                   
         SPACE 2                                                                
TRAPERR  EQU   ERREX                                                            
         EJECT                                                                  
*                              GET LOWEST EST HDR FOR START/END DATES           
LTSTEST  NTR1                                                                   
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBSELAM                                                 
         MVC   KEY+2(2),NBSELAGY                                                
         MVC   KEY+4(3),NBSELPRD                                                
         CLI   NBSELPRD,0                                                       
         BNE   *+10                                                             
         MVC   KEY+4(3),=C'POL'                                                 
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
LTS5     GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   LTSERR                                                           
         OC    KEY+8(4),KEY+8                                                   
         BNZ   LTSERR                                                           
LTS10    MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R3,NBAIO                                                         
         USING ESTHDR,R3                                                        
         CLC   ESTART,NBSELEND                                                  
         BH    LTS12                                                            
         CLC   ESTART,NBSELSTR                                                  
         BNL   LTS15                                                            
LTS12    MVI   KEY+8,X'FF'                                                      
         B     LTS5                                                             
LTS15    MVC   NDDEMOS,EDEMLST                                                  
         NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         B     XMOD                                                             
*                                                                               
LTSERR   MVI   ERROR,INVALID                                                    
         LA    R2,SPLSTRTH                                                      
         GOTO1 TRAPERR                                                          
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
MYWORKD  DSECT                     *** MY WORK DSECT USING ANETWS2 ***          
CLISTSV  DS    CL1020                                                           
CLTNMSV  DS    CL20                                                             
RELO     DS    F                                                                
MKGDMDGD DS    CL1                                                              
OVERIDE  DS    CL1                                                              
PROGNAM  DS    CL1                                                              
*                                                                               
         SPACE 3                                                                
       ++INCLUDE NETINCLS                                                       
NDBLK  DSECT                                                                    
       ++INCLUDE NETDEMOT                                                       
         PRINT OFF                                                              
         SPACE 2                                                                
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE8D                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEMED48   04/14/04'                                      
         END                                                                    
