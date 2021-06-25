*          DATA SET SPTRA97    AT LEVEL 044 AS OF 01/20/16                      
*PHASE T21697C                                                                  
         TITLE 'T21697 - COMMERCIAL INSTRUCTIONS ERROR LIST'                    
***********************************************************************         
*                                                                     *         
*  LEV 37    APR14/89 PRINT ESTIMATE AND DROP RLC-RUN LIMIT CODE      *         
*  LEV 38    SEP06/89 MAX 369 DAYS IN PERIOD                          *         
*  LEV 39    SEP27/89 MAX 371 DAYS IN PERIOD                          *         
*  LEV 40    MAR19/93 ADD CABLE HEAD CODE                             *         
*  LEV 42    MAY17/04 CLEAR P2 (HAS MIDLINE INFO FROM REPORT)         *         
*  LEV 44    NOV03/15 CHANGES FOR OPTICA                              *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
T21697   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21697,RA,RR=R8                                                
         SPACE                                                                  
         ST    R8,RELO                                                          
         SPACE                                                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
*                                                                               
         CLI   PQSW,2              TEST PRINTQ OPEN                             
         BE    GEN0                                                             
         ICM   RE,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,MCVREMOT-MASTD(,RE)                                           
         USING REMOTED,RF                                                       
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVI   REMOTCPY,1                                                       
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'PDF'                                                 
         MVC   REMOTDST,TWAORIG                                                 
         DROP  RF                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
         MVI   PQSW,2                                                           
         B     GEN0                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
GENERR   GOTO1 ERREX                                                            
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
GEN0     DS    0H                                                               
*                                                                               
GEN2     LA    R2,ERRFILE                                                       
         TM    WHEN,X'20'          TEST F...ING SOON                            
         BZ    *+10                                                             
         MVC   40(8,R2),=CL8'TALWRK'                                            
         OPEN  ((R2),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,ENDFILE          RELOCATE EOD ADDRESS                         
         STCM  RE,7,ERRFILE+33                                                  
*                                                                               
         MVC   ELEM(7),=C'NO DATA'                                              
         LA    R7,ELEM                                                          
         USING EFRECD,R7                                                        
                                                                                
* CLEAR HEAD AND MIDLINES FROM INST *                                           
                                                                                
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         MVC   P2,SPACES                                                        
*                                                                               
         LA    RE,HDHK                                                          
         ST    RE,HEADHOOK                                                      
         LA    RE,HDSPECS                                                       
         ST    RE,SPECS                                                         
         XC    MIDHOOK,MIDHOOK                                                  
         XC    FOOTHOOK,FOOTHOOK                                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     ELGET                                                            
*                                                                               
ENDFILE  DS    0H                                                               
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         JE    ENDF2               NO                                           
         CLC   ELEM(7),=C'NO DATA' TEST DIDN'T PRINT ANYTHING                   
         JE    ENDF2                                                            
         MVI   LINE,2                                                           
         MVC   P1(26),=C'*** END OF DDS MESSAGE ***'                            
         MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
ENDF2    LA    R2,ERRFILE                                                       
         CLOSE ((2),)                                                           
         FREEPOOL (R2)                                                          
* CLEAR SO CALLING PROGRAM DOESN'T GET BLINDSIDED                               
         XC    HEADHOOK,HEADHOOK                                                
         MVI   LINE,2                                                           
         MVI   FORCEHED,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
ELGET    LA    R1,ERRFILE                                                       
         GET   (1),(R7)                                                         
*                                                                               
EL20     CLI   FORCEHED,C'Y'       TEST FIRST TIME                              
         JNE   EL21                                                             
*                                                                               
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         JE    EL21                NO                                           
         CLI   PQSW,2              TEST PRTQUE OPEN                             
         BE    EL20A               YES                                          
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         L     RF,MCVREMOT-MASTD(RE)                                            
         USING REMOTED,RF                                                       
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'PDF'                                                 
         MVC   REMOTDST,TWAORIG                                                 
         DROP  RE                                                               
*                                                                               
         GOTO1 OPENPQ              OPEN PRTQ                                    
         MVI   PQSW,2                                                           
*                                                                               
EL20A    BRAS  RE,SNDOPTIC         SEND EDICT= CARDS                            
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
EL21     LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PLMED,EFMED                                                      
         MVC   PLCLT,EFCLT                                                      
         MVC   PLPRDLN,EFPRDLN                                                  
         MVC   PLPRDLN2,EFPRDLN2                                                
         MVC   PLCOPY,EFCOPY                                                    
         SPACE                                                                  
         CLI   EFT0PR11,C'E'       COPY CODE = ESTIMATE                         
         BNE   EL22                                                             
         ZIC   R0,EFCOPY                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLCOPY-1(3),DUB                                                  
         SPACE                                                                  
EL22     MVC   PLMKT,EFMKT                                                      
         BAS   RE,GETMKT                                                        
         MVC   PLMKTNM,WORK                                                     
         MVC   PLSTA(5),EFSTA                                                   
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,(X'80',EFBMKST),WORK,WORK+4                          
         SPACE                                                                  
         CLC   WORK+9(3),SPACES    CABLE HEAD                                   
         BE    *+14                 NO                                          
         MVC   PLSTA(8),WORK+4                                                  
         MVI   PLSTA+4,C'/'                                                     
         SPACE                                                                  
         MVC   PLAFFL,EFAFFL                                                    
         MVC   PLTYPE,EFTYPE                                                    
         CLI   EFREASON,0          MISSING PATTERN                              
         BE    EL28                                                             
         CLI   EFREASON,1          TOO MANY PATTERNS                            
         BE    EL26                                                             
         CLI   EFREASON,2          AUTO PB PTTN ERR                             
         BE    EL24                                                             
         CLI   EFREASON,3          PTTN WITH ALL CMLS DELETED                   
         BE    EL23                                                             
         CLI   EFREASON,4          MORE THAN 371 DAYS IN PERIOD                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PLERR(21),=CL21'MAX 371 DAY PERIOD'                              
         B     EL30                                                             
EL23     MVC   PLERR(21),=CL21'PTTN ALL CMLS DELETED'                           
         B     EL30                                                             
EL24     MVC   PLERR(21),=CL21'AUTO PB PATTERN ERROR'                           
         B     EL30                                                             
EL26     MVC   PLERR(21),=CL21'MORE THAN 27 PATTERNS'                           
         B     EL30                                                             
EL28     MVC   PLERR(10),=C'NO PATTERN'                                         
         OC    EFERRDT,EFERRDT                                                  
         BE    EL30                                                             
         MVC   PLERR+11(3),=C'FOR'                                              
         MVC   PLERR+15(8),EFERRDT                                              
*                                                                               
EL30     DS    0H                                                               
         LH    R0,EFPAGE                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLPAGE,DUB                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     ELGET                                                            
         EJECT                                                                  
GETMKT   NTR1                                                                   
         MVC   WORK,SPACES                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),EFMED                                                   
         MVC   KEY+2(4),EFMKT                                                   
         MVC   KEY+6(2),EFAGY                                                   
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO                                                           
         CLC   KEY(8),0(R6)                                                     
         BE    GETMKTX                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
*                                                                               
         CLC   KEY(8),0(R6)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING MKTRECD,R6                                                       
*                                                                               
GETMKTX  MVC   WORK(20),MKTNAME                                                 
         DROP  R6                                                               
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
ERRFILE  DCB   DDNAME=ERRFILE,DSORG=PS,RECFM=FB,LRECL=100,             X        
               BLKSIZE=2000,MACRF=GM,EODAD=ENDFILE                              
         PRINT NOGEN                                                            
         EJECT                                                                  
HDHK     NTR1  BASE=*,LABEL=*                                                   
         MVC   H1+14(L'EFQUESTR),EFQUESTR                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
SNDOPTIC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVI   LINE,2              SUPPRESS HEADLINES                           
         MVC   EDIORIG,AGYORIG                                                  
         MVC   EDIHDR,=CL5'*HDR*'                                               
         MVC   EDIEDICT(13),=C'EDICT=*OPTICA'                                   
         MVI   EDIWIDE,C'L'        SET FOR LANDSCAPE                            
         MVI   EDIPAGE,C'P'                                                     
         MVI   EDITTYPE,EDITPDFQ                                                
         MVI   ALLOWLIN,0                                                       
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(5),=C'++DDS'                                            
         MVC   EDIDDSID+11(3),=C'SUB'                                           
         MVC   EDIDDSID+15(3),=C'SAI'                                           
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(5),=C'++DDS'                                            
         MVC   EDIIDEN(3),=C'UID'                                               
         MVC   EDICOMN,SVUNIQID                                                 
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
         J     EXIT                                                             
         EJECT                                                                  
HDSPECS  DS    0D                                                               
         SSPEC H1,2,C'REQUESTOR -'                                              
         SSPEC H2,2,AGYNAME                                                     
         SSPEC H3,2,AGYADD                                                      
         SSPEC H1,36,C'COMMERCIAL INSTRUCTIONS ERROR LISTING'                   
         SSPEC H2,36,C'-------------------------------------'                   
         SSPEC H1,85,REPORT                                                     
         SSPEC H2,85,RUN                                                        
         SSPEC H3,85,PAGE                                                       
         SSPEC H5,02,C'MED  CLT  PRD-SLN  PTR-SLN  CPY  MKT'                    
         SSPEC H5,41,C'MARKET NAME        STATION  AFFL TYPE'                   
         SSPEC H5,80,C'REASON'                                                  
         SSPEC H5,105,C'PAGE'                                                   
         SSPEC H6,02,C'---  ---  -------  -------  ---  ---'                    
         SSPEC H6,41,C'-----------        -------  ---- ----'                   
         SSPEC H6,80,C'------'                                                  
         SSPEC H6,105,C'----'                                                   
         DC    X'00'                                                            
         EJECT                                                                  
* DSECT FOR PRINT LINE DATA *                                                   
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL2                                                              
PLMED    DS    CL1                                                              
         DS    CL3                                                              
PLCLT    DS    CL3                                                              
         DS    CL2                                                              
PLPRDLN  DS    CL7                                                              
         DS    CL2                                                              
PLPRDLN2 DS    CL7                                                              
         DS    CL3                                                              
PLCOPY   DS    CL1                                                              
         DS    CL3                                                              
PLMKT    DS    CL4                                                              
         DS    CL2                                                              
PLMKTNM  DS    CL17                                                             
         DS    CL2                                                              
PLSTA    DS    CL7                                                              
         DS    CL2                                                              
PLAFFL   DS    CL3                                                              
         DS    CL3                                                              
PLTYPE   DS    CL1                                                              
         DS    CL4                                                              
PLERR    DS    CL23           NO PATTERN FOR JUL12/84                           
*                             MORE THAN 27 PATTERNS                             
         DS    CL2                                                              
PLPAGE   DS    CL4                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
SPOOLD   DSECT                                                                  
*                                                                               
         ORG   P                                                                
       ++INCLUDE EDIDDSHD                                                       
         ORG   P                                                                
       ++INCLUDE EDIDESTD                                                       
         ORG   P                                                                
       ++INCLUDE EDILINKD                                                       
       ++INCLUDE DDREPMASTD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF7D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAINSTD                                                     
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPTRA97   01/20/16'                                      
         END                                                                    
