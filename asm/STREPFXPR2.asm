*          DATA SET STREPFXPR2 AT LEVEL 005 AS OF 07/31/96                      
*PHASE SPFX02V                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'PURGE MARKETS'                                                  
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02                                                           
         NMOD1 0,SPFX02,R8                                                      
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
FX       DS    0H                                                               
         LA    R0,MKTLSMAX                                                      
         GOTO1 ,BINPARMS,,MKTLST,0,L'MRKET,(0,L'MRKET),(R0)                     
******   XC    TYPACNT(TYPCNTLQ),TYPACNT                                        
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R5,KEY                                                           
         USING MKTRECD,R5                                                       
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMED                                                     
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     FX20                                                             
*                                                                               
FX10     GOTO1 SEQSTA                                                           
*                                                                               
FX20     DS    0H                                                               
         L     R5,ADSTAT           ADSTAT-->RECORD FOUND.                       
         CLC   MKTKTYPE(2),SAVEKEY TYPE(1),MEDIA(1)                             
         BNE   FX100                                                            
         CLC   MKTKAGY,QAGY        CHECK IF RIGHT AGENCY                        
         BNE   FX10                                                             
         CLC   =C'*PURGE',MKTNAME                                               
         BNE   FX10                                                             
*                                                                               
         L     R1,TYPMCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TYPMCNT                                                       
         OI    MKTCNTL,X'80'                                                    
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(15),0(R5)                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX30                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',SAVEKEY,ADSTAT                
FX30     MVC   P(15),0(R5)                                                      
         GOTO1 HEXOUT,DMCB,15(R5),P+20,3,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
* PUT MARKET IN TABLE                                                           
         MVC   P(4),MKTKMKT                                                     
         PACK  DUB,MKTKMKT                                                      
         CVB   R2,DUB                                                           
         STCM  R2,3,MRKET                                                       
         GOTO1 HEXOUT,DMCB,MRKET,P+10,2,=C'TOG'                                 
         GOTO1 REPORT                                                           
         GOTO1 BINSRCH,BINPARMS,(1,MRKET)                                       
         OC    BINPARMS,BINPARMS   TABLE FULL?                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   BINPARMS,1          SHOULDN'T BE IN TABLE ALREADY                
         BE    FX10                                                             
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
* DELETE THE MKT/STA RECORDS                                                    
FX100    DS    0H                                                               
         LA    R6,MKTLST                                                        
         L     R4,BINPARMS+8       NUMBER OF MARKETS IN TABLE                   
*                                                                               
FX110    XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING SFXRECD,R5                                                       
         MVI   SFXKTYPE,C'N'                                                    
         MVC   SFXKAGCY,QAGY                                                    
         MVC   SFXKMED,QMED                                                     
         MVC   SFXKMKCL(2),0(R6)    MARKET NUMBER                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     FX130                                                            
*                                                                               
FX120    GOTO1 SEQSTA                                                           
*                                                                               
FX130    DS    0H                                                               
         L     R5,ADSTAT              ADSTAT-->RECORD FOUND.                    
         CLC   SFXKTYPE(4),SAVEKEY    TYP(1),AGY(2),MED(1)                      
         BNE   FX200                                                            
         CLC   SFXKMKCL(2),SAVEKEY+4  PMKT(2)                                   
         BNE   FX150                                                            
*                                                                               
* DELETE MKT/STA RECORD                                                         
         L     R1,TYPNCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TYPNCNT                                                       
         OI    SFXCNTL,X'80'                                                    
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(15),0(R5)                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX140                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',SAVEKEY,ADSTAT                
FX140    GOTO1 HEXOUT,DMCB,(R5),P,18,=C'TOG'                                    
         GOTO1 REPORT                                                           
         B     FX120                                                            
*                                                                               
FX150    LA    R6,2(R6)            CHANGE MARKET                                
         BCT   R4,FX110                                                         
         DROP  R5                                                               
*                                                                               
* DELETE THE MASTER RECORDS                                                     
FX200    DS    0H                                                               
         LA    R0,TABLEMAX                                                      
         GOTO1 ,BINPARM2,,TABLE,0,L'STATN,(0,L'STATN),(R0)                      
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING STARECD,R5                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     FX220                                                            
*                                                                               
FX210    GOTO1 SEQSTA                                                           
*                                                                               
FX220    DS    0H                                                               
         L     R5,ADSTAT           ADSTAT-->RECORD FOUND.                       
         CLC   STAKTYPE(2),SAVEKEY TYP(1),MED(1)                                
         BNE   FX300                                                            
         CLC   STAKAGY,QAGY                                                     
         BNE   FX210                                                            
*                                                                               
* LOOK FOR MARKET                                                               
         PACK  DUB,SMKT                                                         
         CVB   R2,DUB                                                           
         STCM  R2,3,MRKET                                                       
         GOTO1 BINSRCH,BINPARMS,(0,MRKET)                                       
         CLI   BINPARMS,1          CHECK IF MARKET IN TABLE                     
         BE    FX210               NO                                           
                                                                                
* DELETE MASTER RECORD                                                          
         L     R1,TYPSCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TYPSCNT                                                       
         OI    SCNTL,X'80'                                                      
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(15),0(R5)                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX230                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',SAVEKEY,ADSTAT                
FX230    MVC   P(15),0(R5)                                                      
         GOTO1 HEXOUT,DMCB,15(R5),P+20,3,=C'TOG'                                
         GOTO1 REPORT                                                           
*                                                                               
* PUT STATION IN TABLE                                                          
         GOTO1 BINSRCH,BINPARM2,(1,STAKCALL)                                    
         OC    BINPARM2,BINPARM2   TABLE FULL?                                  
         BNZ   FX210                                                            
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
* DELETE THE ADDRESS RECORDS                                                    
FX300    DS    0H                                                               
         LA    R6,TABLE                                                         
         L     R4,BINPARM2+8       NUMBER OF STATIONS IN TABLE                  
*                                                                               
FX310    XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING ADRRECD,R5                                                       
         MVI   ADDKTYPE,ADDKTYPQ                                                
         MVC   ADDKMED,QMED                                                     
         MVC   ADDKCALL,0(R6)                                                   
         MVC   ADDKAGY,QAGY                                                     
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
*                                                                               
FX320    DS    0H                                                               
         L     R5,ADSTAT               ADSTAT-->RECORD FOUND.                   
         CLC   ADDKTYPE(2),SAVEKEY     TYP(1),MED(1)                            
         BNE   FX400                                                            
         CLC   ADDKCALL(7),SAVEKEY+2   STATION(5),AGY(2)                        
         BNE   FX340                                                            
                                                                                
* DELETE ADDRESS RECORD                                                         
         L     R1,TYPACNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TYPACNT                                                       
         OI    ACNTL,X'80'                                                      
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(15),0(R5)                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX330                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',SAVEKEY,ADSTAT                
FX330    MVC   P(15),0(R5)                                                      
         GOTO1 HEXOUT,DMCB,15(R5),P+20,3,=C'TOG'                                
         GOTO1 REPORT                                                           
FX340    LA    R6,5(R6)                                                         
         BCT   R4,FX310                                                         
         DROP  R5                                                               
                                                                                
*                                                                               
* DELETE THE X POINTERS                                                         
FX400    DS    0H                                                               
         LA    R6,TABLE                                                         
         L     R4,BINPARM2+8       NUMBER OF STATIONS IN TABLE                  
*                                                                               
FX410    XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STARECD,R5                                                       
         MVI   STXKTYPE,C'X'                                                    
         MVC   STXKAGY,QAGY                                                     
         MVC   STXKSTA,0(R6)                                                    
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     FX420                                                            
*                                                                               
FX420    DS    0H                                                               
         L     R5,ADSTAT               ADSTAT-->RECORD FOUND.                   
         CLC   STXKTYPE(3),SAVEKEY     TYP(1),AGY(1)                            
         BNE   FX500                                                            
         CLC   STXKSTA(5),SAVEKEY+3    STATION(5)                               
         BNE   FX440                                                            
                                                                                
* DELETE ADDRESS RECORD                                                         
         L     R1,TYPXCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TYPXCNT                                                       
         OI    SCNTL,X'80'                                                      
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(15),0(R5)                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX430                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',SAVEKEY,ADSTAT                
FX430    MVC   P(8),0(R5)                                                       
         GOTO1 HEXOUT,DMCB,8(R5),P+10,10,=C'TOG'                                
         GOTO1 REPORT                                                           
FX440    LA    R6,5(R6)                                                         
         BCT   R4,FX410                                                         
         DROP  R5                                                               
*                                                                               
* DELETE THE FORMAT RECORDS                                                     
FX500    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STARECD,R5                                                       
         MVI   STFKTYPE,C'F'                                                    
         MVC   STFKAGY,QAGY                                                     
         MVC   STFKMED,QMED                                                     
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     FX530                                                            
*                                                                               
FX520    GOTO1 SEQSTA                                                           
*                                                                               
FX530    DS    0H                                                               
         L     R5,ADSTAT              ADSTAT-->RECORD FOUND.                    
         CLC   STFKTYPE(4),SAVEKEY    TYP(1),AGY(2),MED(1)                      
         BNE   FX600                                                            
         GOTO1 BINSRCH,BINPARMS,(0,STFKMS)                                      
         CLI   BINPARMS,1          CHECK IF MARKET IN TABLE                     
         BE    FX520               NO                                           
*                                                                               
         L     R1,TYPFCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TYPFCNT                                                       
         OI    SCNTL,X'80'                                                      
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(15),0(R5)                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX540                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',SAVEKEY,ADSTAT                
FX540    MVC   P(8),0(R5)                                                       
         GOTO1 HEXOUT,DMCB,8(R5),P+10,10,=C'TOG'                                
         GOTO1 REPORT                                                           
         B     FX520                                                            
         DROP  R5                                                               
*                                                                               
* DELETE THE L RECORDS                                                          
FX600    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING ANMRECD,R5                                                       
         MVI   ANMKTYPE,ANMKTYPQ                                                
         MVC   ANMKAGCY,QAGY                                                    
         MVC   ANMKMED,QMED                                                     
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     FX630                                                            
*                                                                               
FX620    GOTO1 SEQSTA                                                           
*                                                                               
FX630    DS    0H                                                               
         L     R5,ADSTAT              ADSTAT-->RECORD FOUND.                    
         CLC   ANMKTYPE(4),SAVEKEY    TYP(1),AGY(2),MED(1)                      
         BNE   FX700                                                            
         PACK  DUB,ANMKNMRK                                                     
         CVB   R2,DUB                                                           
         STCM  R2,3,MRKET                                                       
         GOTO1 BINSRCH,BINPARMS,(0,MRKET)                                       
         CLI   BINPARMS,1          CHECK IF MARKET IN TABLE                     
         BE    FX620               NO                                           
*                                                                               
         L     R1,TYPLCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TYPLCNT                                                       
         OI    ANMCNTL,X'80'                                                    
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(15),0(R5)                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX640                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',SAVEKEY,ADSTAT                
FX640    MVC   P(15),0(R5)                                                      
         GOTO1 HEXOUT,DMCB,15(R5),P+20,3,=C'TOG'                                
         GOTO1 REPORT                                                           
         B     FX620                                                            
         DROP  R5                                                               
*                                                                               
FX700    MVC   P1(18),=C'M-TYPE DELETED  = '                                    
         EDIT  TYPMCNT,(10,P1+25),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK            
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         MVC   P1(18),=C'N-TYPE DELETED  = '                                    
         EDIT  TYPNCNT,(10,P1+25),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK            
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         MVC   P1(18),=C'S-TYPE DELETED  = '                                    
         EDIT  TYPSCNT,(10,P1+25),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK            
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         MVC   P1(18),=C'A-TYPE DELETED  = '                                    
         EDIT  TYPACNT,(10,P1+25),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK            
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         MVC   P1(18),=C'F-TYPE DELETED  = '                                    
         EDIT  TYPFCNT,(10,P1+25),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK            
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         MVC   P1(18),=C'X-TYPE DELETED  = '                                    
         EDIT  TYPXCNT,(10,P1+25),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK            
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         MVC   P1(18),=C'L-TYPE DELETED  = '                                    
         EDIT  TYPLCNT,(10,P1+25),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK            
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
         GOTO1 AENDREQ                                                          
***********************************************************************         
         LTORG                                                                  
BINPARMS DS    6F                  BINSRCH PARAMETERS - MARKETS                 
BINPARM2 DS    6F                  BINSRCH PARAMETERS - STATIONS                
TYPACNT  DS    F                   # OF TYPE-'A' RECORDS.                       
TYPFCNT  DS    F                   # OF TYPE-'F' RECORDS.                       
TYPLCNT  DS    F                   # OF TYPE-'L' RECORDS.                       
TYPMCNT  DS    F                   # OF TYPE-'M' RECORDS.                       
TYPNCNT  DS    F                   # OF TYPE-'N' RECORDS.                       
TYPSCNT  DS    F                   # OF TYPE-'S' RECORDS.                       
TYPXCNT  DS    F                   # OF TYPE-'X' RECORDS.                       
TYPCNTLQ EQU   *-TYPACNT           LENGTH OF COUNTS                             
MRKET    DS    CL2                                                              
STATN    DS    CL5                                                              
SAVEKEY  DS    XL32                                                             
MKTLST   DS    500XL(L'MRKET)                                                   
MKTLSMAX EQU   (*-MKTLST)/(L'MRKET)      MAX ENTRIES                            
TABLE    DS    1000XL(L'STATN)                                                  
TABLEMAX EQU   (*-TABLE)/(L'STATN)       MAX ENTRIES                            
         EJECT                                                                  
************************* FIXED-RECORDS DSECT *************************         
       ++INCLUDE SPGENANMK                                                      
         EJECT                                                                  
       ++INCLUDE SPGENMSTA                                                      
         EJECT                                                                  
       ++INCLUDE SPGENADD1                                                      
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005STREPFXPR207/31/96'                                      
         END                                                                    
