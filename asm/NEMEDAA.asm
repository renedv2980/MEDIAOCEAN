*          DATA SET NEMEDAA    AT LEVEL 050 AS OF 10/01/13                      
*          DATA SET NEMEDAA    AT LEVEL 025 AS OF 12/10/99                      
*PHASE T31EAAA,+0                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CLUNPK                                                                 
         TITLE 'T31EAA - FILE FIX MODULE'                                       
         PRINT NOGEN                                                            
************************************************************                    
* NETWORK FILEFIX                                                               
*   THIS PROGRAM UPDATES UNITS WITH STATION TYPE AND POSTING TYPE               
*   FROM THE STATION MASTER RECORD.                                             
*                                                                               
**************************************************************                  
T31EAA   CSECT                                                                  
         NMOD1 0,**UNFX**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         MVI   UVLPRT,C'N'         FORCE THIS TO NO                             
*                                  ELSE IT DIES WHEN CLOSING APP                
*                                  WHEN RUN OVERNIGHT - SAVE FOR                
*                                  TSO TESTING                                  
                                                                                
         CLI   UVLTYP,C'X'         NEW BILLING REC XTRACT                       
         BNE   FX00                                                             
         BAS   RE,CHKBILLS                                                      
         B     XIT                                                              
FX00     EQU   *                                                                
                                                                                
                                                                                
* -> LOAD STATION TABLE                                                         
         XC    COUNTER,COUNTER                                                  
         L     R2,AIO                                                           
         USING STAREC,R2                                                        
         LA    R3,STATBL           CL4 STA / CL1 STATYPE                        
         LA    R4,1000                                                          
         NETGO NVSETSTA,DMCB                                                    
         MVC   FILENAME,=C'STATION '                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'N'                                                       
         GOTO1 HIGH                                                             
         B     FX2                                                              
FX3      GOTO1 SEQ                                                              
*                                                                               
FX2      CLC   KEY(2),KEYSAVE          TYPE/MEDIA                               
         BNE   FX5                                                              
         CLC   STAKAGY,NBSELAGY       AGY                                       
         BNE   FX3                                                              
         MVC   0(4,R3),STAKCALL                                                 
         MVC   4(1,R3),STYPE       STATION TYPE                                 
         MVC   5(1,R3),SPTYPE                                                   
         CLI   5(R3),X'40'         IF POSTING TYPE BLANK                        
         BH    *+10                                                             
         MVC   5(1,R3),4(R3)       USE STATION TYPE                             
         MVC   6(1,R3),SOVBKTYP    OVERRIDE BOOK TYPE                           
         CLI   6(R3),X'40'         IF NO BKTYP                                  
         BH    *+10                                                             
         MVC   6(1,R3),5(R3)       USE POST TYPE                                
         MVC   7(1,R3),STRTYPE     OVERRIDE TRAFFIC TYPE                        
         CLI   7(R3),X'40'         IF NO TRAFFIC TYPE                           
         BH    *+10                                                             
         MVC   7(1,R3),5(R3)       USE POST TYPE                                
         MVC   8(1,R3),SUBMEDIA    SUB MEDIA                                    
FX2B     L     R1,AIO                                                           
         MVC   P(100),0(R1)                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,9(R3)                                                         
         BCT   R4,FX3                                                           
         MVC   P+1(32),=C'STATION TABLE FULL *************'                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     FX5                                                              
*                                                                               
         EJECT                                                                  
* -> PREPARE TO READ UNIT FILE                                                  
FX5      NETGO NVSETUNT,DMCB                                                    
         L     R1,NBAIO          SET NBAIO=AIO                                  
         ST    R1,AIO                                                           
                                                                                
* SET UP AND READ X'84' KEY                                                     
         XC    KEY,KEY           CLEAR KEY                                      
         LA    R4,1                  NOTE *** R4 RESERVED ***                   
         LA    R2,KEY                                                           
         USING NUKPKEY,R2                                                       
         MVI   0(R2),X'84'                                                      
         MVC   NUKPAM,NBACTAM                                                   
         CLC   =C'ALL',NBSELCLI                                                 
         BE    FX6                                                              
         MVC   NUKPCLT,NBEFFCLI                                                 
         CLC   =C'ALL',NBSELNET                                                 
         BE    FX6                                                              
         CLI   NBSELNET,X'40'                                                   
         BNH   FX6                                                              
         MVC   NUKPNET,NBSELNET                                                 
*                                                                               
FX6      MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                   
         B     FX6D                                                             
                                                                                
FXSEQ    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR  ',KEY,KEY,0                   
*                                                                               
FX6D     DS    0H                                                               
         CLC   KEY(2),KEYSAVE                                                   
         BNE   FXEND                                                            
         CLC   =C'ALL',NBSELCLI                                                 
         BE    FX8                                                              
         CLC   KEY(4),KEYSAVE                                                   
         BNE   FXEND                                                            
FX8      BAS   RE,KEYFILT        DO WE WANT THIS UNIT?                          
         BNE   FXSEQ               NO                                           
         B     MAINLINE            YES                                          
                                                                                
*                                                                               
FXEND    MVC   P(13),=C'UNITS UPDATED'                                          
         L     R3,COUNTER                                                       
         EDIT  (R3),(7,P+14)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
MAINLINE DS    0H                                                               
                                                                                
* -> UPDATE 02 ELEM WITH STA/POSTYPE FROM MASTER RECORD                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFILE ',KEY+21,AIO,DMWORK           
                                                                                
         L     R2,AIO              FOR GETEL                                    
         USING NURECD,R2                                                        
         CLI   NBSELPAK,0          PACKAGE FILTER?                              
         BE    *+14                                                             
         CLC   NBSELPAK,NUPACK                                                  
         BNE   FXSEQ                                                            
                                                                                
         LA    R3,STATBL           MATCH NETWORK WITH STATABLE                  
         LA    R1,1000             1000=MAX NUMBER OF NETWORKS                  
FX7      CLC   NUKNET,0(R3)                                                     
         BE    FX10                                                             
         LA    R3,9(R3)                                                         
         BCT   R1,FX7                                                           
         BAS   RE,FXMISS                                                        
         B     XIT                                                              
FX10     MVI   ELCODE,2            IS THERE 02 ELEM                             
         MVC   NBDTADSP,=X'001B'                                                
         BAS   RE,GETEL                                                         
         BE    FX12                YES                                          
         DC    H'0'                                                             
         BAS   RE,FXELEM           NO ,ADD IT                                   
         BAS   RE,SETRSTAT         SET TRAFFIC STATUS                           
         CLI   UVLTYP,C'T'         TRAFFIC TYPE ONLY?                           
         BE    FX16                YES                                          
         USING NUSDRD,R2                                                        
         CLI   DMCB+12,0           WAS IT ADDED                                 
         BE    FX15                YES                                          
         BAS   RE,FXMISS2          NO/ERROR MESSAGE                             
         B     XIT                                                              
                                                                                
FX12     CLI   UVLTYP,C'P'        POSTING TYPE ONLY ?                           
         BE    FX14               YES                                           
         BAS   RE,SETRSTAT        SET TRAFFIC STATUS                            
         CLI   UVLTYP,C'T'        TRAFFIC TYPE ONLY?                            
         BE    FX16               YES                                           
         MVC   NUSTATYP,4(R3)                                                   
         CLI   UVLTYP,C'S'        STATION TYPE ONLY?                            
         BE    FX15               YES                                           
FX14     MVC   NUPOSTYP,5(R3)                                                   
         MVC   NUBKTYP,6(R3)                                                    
         MVC   NUSDSBMD,8(R3)                                                   
*                                                                               
FX15     DS    0H                                                               
* -> ADJUST DEMO PRECISION FOR POSTING TYPE                                     
         CLI   NUPOSTYP,C'N'                                                    
         BE    FX15B                                                            
         CLI   NUPOSTYP,C'H'                                                    
         BE    FX15B                                                            
         CLI   NUPOSTYP,C'S'                                                    
         BNE   FX15C                                                            
FX15B    BAS   RE,FIXNS                                                         
         B     FX16                                                             
*                                                                               
FX15C    CLI   NUPOSTYP,C'C'                                                    
         BE    FX15D                                                            
         CLI   NUPOSTYP,C'O'                                                    
         BE    FX15D                                                            
         CLI   NUPOSTYP,C'D'       AND LET'S ADD RADIO                          
         BNE   FX16                                                             
FX15D    BAS   RE,FIXCO                                                         
                                                                                
FX16     CLI   UVLTST,C'Y'         DO WE WRITE RECORD BACK?                     
         BE    FX17                                                             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'UNTFILE ',KEY+21,AIO,DMWORK           
*                                                                               
FX17     L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
                                                                                
*                                                                               
         CLI   UVLPRT,C'Y'         PRINT RECORD                                 
         BNE   FIXKEY                                                           
         L     R2,NBAIO                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'PRINT',(R2),C'DUMP',30,=C'1D'                 
*                                                                               
         EJECT                                                                  
*************************************************************                   
* IF STATION STATUS WAS UPDATED - UPDATE STATION STATUS ON KEY                  
*                                                                               
FIXKEY   DS    0H                                                               
                                                                                
         CLI   UVLTYP,C'P'        POSTING TYPE ONLY                             
         BE    FXSEQ                                                            
                                                                                
*->  UPDATE STATUS ON X'04' KEY                                                 
         MVC   KEYSV,KEY           SAVE X'84' KEY                               
         L     R1,NBAIO            GET X'04' KEY                                
         MVC   KEY,0(R1)                                                        
         MVC   FILENAME,=C'UNTDIR  '                                            
         MVI   USEIO,C'N'                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
                                                                                
****     GOTO1 =V(PRNTBL),DMCB,=C'KEY1',KEY,C'DUMP',40,=C'1D'                   
                                                                                
         LA    R1,KEY                                                           
         USING NUKEY,R1                                                         
         NI    NUKSTAT,X'FF'-X'07' CLEAR STATION TYPE BITS                      
         CLI   RSTAT,C'N'          NETWORK = 0                                  
         BE    FIX8                                                             
         CLI   RSTAT,C'H'          HISPANIC = NETWORK                           
         BE    FIX8                                                             
         CLI   RSTAT,C'S'          SYNDICATION = 02                             
         BNE   *+12                                                             
         OI    NUKSTAT,X'02'                                                    
         B     FIX8                                                             
         CLI   RSTAT,C'C'          CABLE = 01                                   
         BNE   *+12                                                             
         OI    NUKSTAT,X'01'                                                    
         B     FIX8                                                             
         CLI   RSTAT,C'V'          VIDEO = 04                                   
         BNE   *+12                                                             
         OI    NUKSTAT,X'04'                                                    
         B     FIX8                                                             
         OI    NUKSTAT,X'03'        DEFAULT = 03                                
FIX8     DS    0H                                                               
         MVC   RSTAT,NUKSTAT       SAVE STATUS BYTE                             
         DROP  R1                                                               
*                                                                               
                                                                                
FIX8A    CLI   UVLTST,C'Y'     *** IS IT TEST? ***                              
         BE    FIX8B                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* FOR TESTING ONLY                                                              
FIX8B    DS    0H                                                               
         CLI   UVLPRT,C'Y'         PRINT RECORD                                 
         BNE   FIX8BX                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'KEY',KEY,C'DUMP',40,=C'1D'                    
FIX8BX   EQU   *                                                                
                                                                                
         LA    R1,KEY                                                           
         USING NUKEY,R1                                                         
         MVC   KEY,KEYSV           REST X'84' KEY                               
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
***      GOTO1 =V(PRNTBL),DMCB,=C'KEY2',KEY,C'DUMP',40,=C'1D'                   
         LA    R1,KEY                                                           
         MVC   NUKSTAT,RSTAT       SET IN NEW STATUS                            
         MVI   RSTAT,0             CLEAR STATUS SAVE BYTE                       
         DROP  R1                                                               
*                                                                               
         CLI   UVLTST,C'Y'         WRITE IT BACK?                               
         BE    FIX10                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY,KEY                      
FIX10    DS    0H                                                               
         CLI   UVLPRT,C'Y'         PRINT RECORD                                 
         BNE   FIX10X                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'KEY2',KEY,C'DUMP',30,=C'1D'                   
FIX10X   EQU   *                                                                
*                                                                               
FIXKX    B     FXSEQ                                                            
                                                                                
*                                                                               
COUNTER  DS    F                                                                
         EJECT                                                                  
*                                                                               
FXMISS   NTR1                                                                   
         L     R1,NOSTAT                                                        
         C     R1,=F'10'                                                        
         BH    XIT                                                              
         LA    R1,1(R1)                                                         
         ST    R1,NOSTAT                                                        
         MVC   P(16),=C'NO STATION MATCH'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 HEXOUT,DMCB,(R2),P,50                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
FXMISS2  NTR1                                                                   
         MVC   P(16),=C'ADD ELEM PROBLEM'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 HEXOUT,DMCB,(R2),P,50                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
FXELEM   NTR1                                                                   
         DC    H'0'                                                             
         MVC   AIO,NBAIO                                                        
         XC    ELEM,ELEM                                                        
         MVI   ELEM,2                                                           
         MVI   ELEM+1,X'14'                                                     
         MVC   ELEM+6(1),4(R3)                                                  
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* - CHANGE RECORD STATUS                                                        
SETRSTAT NTR1                                                                   
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
         NI    NURSTAT,X'FF'-X'07'         CLEAR STATION TYPE BITS              
         MVI   RSTAT,C'N'                                                       
         CLI   7(R3),C'N'          NETWORK                                      
         BE    SETRX                                                            
         CLI   7(R3),C'H'          HISPANIC = NETWORK                           
         BE    SETRX                                                            
         CLI   7(R3),C'S'          SYNDICATION                                  
         BNE   SETR10                                                           
         OI    NURSTAT,X'02'                                                    
         MVI   RSTAT,C'S'                                                       
         B     SETRX                                                            
SETR10   CLI   7(R3),C'C'          CABLE                                        
         BNE   SETR20                                                           
         OI    NURSTAT,X'01'                                                    
         MVI   RSTAT,C'C'                                                       
         B     SETRX                                                            
SETR20   CLI   7(R3),C'V'          VIDEO                                        
         BNE   SETR30                                                           
         OI    NURSTAT,X'04'                                                    
         MVI   RSTAT,C'V'                                                       
         B     SETRX                                                            
SETR30   OI    NURSTAT,X'03'       DEFAULT                                      
         MVI   RSTAT,C'O'                                                       
SETRX    DS    0H                                                               
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* IF POSTYPE = N OR S, NSPREC TABLE                                             
*                                                                               
FIXNS    NTR1                                                                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'DD'                                                     
         USING NUOVD,R2                                                         
         MVC   NBDTADSP,=X'001B'                                                
         BAS   RE,GETEL                                                         
         BE    NS10                                                             
NS05     L     R2,NBAIO                                                         
         MVI   ELCODE,X'DE'                                                     
         MVC   NBDTADSP,=X'001B'                                                
         BAS   RE,GETEL                                                         
         BNE   NSX                                                              
NS10     CLI   NUOVFLG,X'80'                IF NAD DEMO                         
         BE    NSNEXT                       SKIP IT                             
*                                                                               
*                                                                               
*        DECODE                    ** NOTE 'BYTE' USED FOR 2ND LIST             
         XC    DBLOCK,DBLOCK       PREPARE BLOCK FOR DE/EN CODE CALLS           
         L     R1,NBACOM                                                        
         LTR   R1,R1                                                            
         BNZ   NS12                                                             
         DC    H'0'                                                             
NS12     MVC   DBCOMFCS,NBACOM                                                  
         L     RF,NBDEMCON                                                      
         LA    R4,NUOVCAT                                                       
         LA    R3,DBLOCK                                                        
         GOTO1 (RF),DMCB,(R4),('DEMOCON_16',(R4)),(R3),BYTE                     
*                                                                               
         CLI   NUOVMOD,C'H'        ...ALL H'S -> T                              
         BNE   *+8                                                              
         MVI   NUOVMOD,C'T'                                                     
*                                                                               
NS15     LA    R3,NSPREC           SET PROPER PRECISION                         
         LA    RE,7                                                             
NS20     CLC   NUOVMOD,0(R3)                                                    
         BE    NS30                                                             
         LA    R3,2(R3)                                                         
         BCT   RE,NS20                                                          
****->   B     NSNEXT                                                           
         B     NS55                NEED TO ENCODE IT                            
*                                                                               
NS30     DS    0H                  SET NEW PRECISION                            
         CLI   0(R3),C'R'          IF R OR P (RATINGS)                          
         BE    NS40                                                             
         CLI   0(R3),C'P'                                                       
         BE    NS40                                                             
         CLI   0(R3),C'T'          AND IF T  (IMPRESSIONS)                      
         BNE   NS50                                                             
NS40     CLC   NUOVPRE,1(R3)        IS PRECISION NET/SYND ?                     
         BE    NS50                 YES/OK                                      
         SR    R0,R0                NO/ ADJUST VALUE                            
         ICM   R1,15,NUOVVAL                                                    
         CLI   0(R3),C'T'          IS IT IMPS?                                  
         BE    *+8                 YES                                          
         LA    R1,5(R1)            NO/ROUND FOR GRPS                            
         D     R0,=F'10'                                                        
         STCM  R1,15,NUOVVAL                                                    
NS50     MVC   NUOVPRE,1(R3)                                                    
*                                                                               
*        ENCODE                    **NOTE 'BYTE' USED FOR 2ND LIST              
NS55     LA    R3,DBLOCK                                                        
         GOTO1 (RF),DMCB,(R4),('DEMOCON_17',(R4)),(R3),BYTE                     
*                                                                               
NSNEXT   BAS   RE,NEXTEL                                                        
         BE    NS10                                                             
         CLI   ELCODE,X'DE'        HAVE WE FINISHED                             
         BNE   NS05                                                             
NSX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* IF POSTYPE = C OR O, COPREC TABLE                                             
*                                                                               
FIXCO    NTR1                                                                   
         L     R2,NBAIO                                                         
         USING NUOVD,R2                                                         
         MVI   ELCODE,X'DD'                                                     
         MVC   NBDTADSP,=X'001B'                                                
         BAS   RE,GETEL                                                         
         BE    CO10                                                             
CO05     L     R2,NBAIO                                                         
         MVC   NBDTADSP,=X'001B'                                                
         MVI   ELCODE,X'DE'                                                     
         BAS   RE,GETEL                                                         
         BNE   COX                                                              
         USING NUOVD,R2                                                         
CO10     CLI   NUOVFLG,X'80'                IF NAD DEMO                         
         BE    CONEXT                       SKIP IT                             
*                                                                               
*****    CLI   ELCODE,X'DD'        FOR DD ELEM                                  
*****    BNE   CO15                                                             
******   DECODE ******                                                          
         L     RF,NBDEMCON                                                      
         LA    R4,NUOVCAT                                                       
         LA    R3,DBLOCK                                                        
         GOTO1 (RF),DMCB,(R4),('DEMOCON_16',(R4)),(R3),BYTE                     
*                                                                               
         CLI   NUOVMOD,C'T'        ALL T'S -> H                                 
         BNE   *+8                                                              
         MVI   NUOVMOD,C'H'                                                     
*                                                                               
CO15     LA    R3,COPREC           SET PROPER PRECISION                         
         LA    R4,7                                                             
CO20     CLC   NUOVMOD,0(R3)                                                    
         BE    CO30                                                             
         LA    R3,2(R3)                                                         
         BCT   R4,CO20                                                          
****     B     CONEXT                                                           
         B     CO55                                                             
*                                                                               
CO30     DS    0H                  SET NEW PRECISION                            
         CLI   0(R3),C'R'          IF R OR P  (RATING)                          
         BE    CO40                                                             
         CLI   0(R3),C'P'                                                       
         BE    CO40                                                             
         CLI   0(R3),C'H'           AND IF H (IMPRESSION)                       
         BNE   CO50                                                             
CO40     CLC   NUOVPRE,1(R3)        IS PRECISION CABLE/OTH?                     
         BE    CO50                 YES                                         
         ICM   R1,15,NUOVVAL        NO - ADJUST VALUE                           
         MH    R1,=H'10'                                                        
         STCM  R1,15,NUOVVAL                                                    
CO50     MVC   NUOVPRE,1(R3)        AND SET NEW PRECISION FACTOR                
*                                                                               
CO55     L     RF,NBDEMCON          *** ENCODE ***                              
         LA    R4,NUOVCAT                                                       
         LA    R3,DBLOCK                                                        
         GOTO1 (RF),DMCB,(R4),('DEMOCON_17',(R4)),(R3),BYTE                     
*                                                                               
CONEXT   BAS   RE,NEXTEL                                                        
         BE    CO10                                                             
         CLI   ELCODE,X'DE'                                                     
         BNE   CO05                                                             
COX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
         SPACE 2                                                                
*                                                                               
NSPREC   DC    C'R',X'81',C'S',X'81',C'P',X'81',C'T',X'43'                      
         DC    C'H',X'43',C'U',X'42',C'V',X'40'                                 
*                                                                               
COPREC   DC    C'R',X'82',C'S',X'81',C'P',X'82',C'T',X'42'                      
         DC    C'H',X'42',C'U',X'42',C'V',X'40'                                 
*                                                                               
         EJECT                                                                  
KEYFILT  NTR1                                                                   
         LA    R2,KEY                                                           
         USING NUKPKEY,R2                                                       
         CLC   =C'ALL',NBSELNET      NET =ALL                                   
         BE    KYF5                                                             
         CLI   NBSELNET,0            NET=000                                    
         BE    KYF5                                                             
         CLC   NUKPNET,NBSELNET                                                 
         BNE   KYNOTOK                                                          
KYF5     OC    BINSTR,BINSTR                                                    
         BNZ   KYF6                                                             
         GOTO1 DATCON,DMCB,NBSELSTR,(2,BINSTR)                                  
         GOTO1 DATCON,DMCB,NBSELEND,(2,BINEND)                                  
KYF6     CLC   NUKPDATE,BINSTR                                                  
         BL    KYNOTOK                                                          
         CLC   NUKPDATE,BINEND                                                  
         BH    KYNOTOK                                                          
*                                                                               
         CLI   NBSELESE,0          ESTIMATE RANGE?                              
         BE    KYF7                                                             
         CLC   NUKPEST,NBSELESE   YES                                           
         BH    KYNOTOK                                                          
         CLC   NUKPEST,NBSELEST                                                 
         BL    KYNOTOK                                                          
         B     KYF8                                                             
*                                                                               
KYF7     CLI   NBSELEST,0          ESTIMATE FILTERING?                          
         BE    KYF8                NO                                           
         CLC   NUKPEST,NBSELEST    YES/ IF LOW, NOT OK                          
         BNE   KYNOTOK                                                          
*                                                                               
KYF8     DS    0H                                                               
*                                                                               
KYOK     SR    R2,R2                                                            
KYNOTOK  LTR   R2,R2         CC NOT = REJECTS UNIT                              
*                                                                               
KYFX     XIT1                                                                   
         EJECT                                                                  
*********************************************************                       
* READ THROUGH NEW BILLING RECS AND MATCH WITH UNIT                             
* PRINT OUT UNIT KEY IF NO MATCH                                                
*                                                                               
CHKBILLS NTR1                                                                   
         LA    R2,KEY2                                                          
         USING NUBKEY,R2                                                        
         MVC   0(2,R2),=X'0E06'                                                 
         MVC   NUBKAM,NBACTAM                                                   
         MVC   KEY2SV,KEY2                                                      
         GOTO1 NBDM,DMCB,(0,=CL8'DMRDHI'),=C'XSPDIR',KEY2,KEY2,0                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
CK10     CLC   KEY2(3),KEY2SV    FOR AGENCY                                     
         BNE   CKX                                                              
         LA    R3,NUBDA                                                         
         L     R4,NBAIO                                                         
         GOTO1 NBDM,DMCB,=CL8'GETREC',=C'XSPFIL  ',(R3),(R4),DMWORK             
         LA    R4,NUBELDQ(R4)                                                   
         USING NBILD,R4                                                         
         TM    NBILST,NBILUBQ      UNBILLED?                                    
         BO    CK40                                                             
         XC    KEY,KEY             DO WE HAVE 84 KEY MATCH?                     
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(18),KEY2+2    SET UP 84 KEY                                
         MVC   KEYSV,KEY                                                        
         GOTO1 NBDM,DMCB,=C'DMRDHI',=C'UNTDIR',KEY,KEY,0                        
         L     R1,NOSTAT           USE AS COUNTER FOR RECS READ                 
         LA    R1,1(R1)                                                         
         ST    R1,NOSTAT                                                        
         CLC   KEY(20),KEYSV       DID WE FIND IT?                              
         BE    CK40                                                             
         L     R1,NO02             USE AS COUNTER FOR RECS NOT FOUND            
         LA    R1,1(R1)                                                         
         ST    R1,NO02                                                          
***      GOTO1 =V(PRNTBL),DMCB,=C'PRINT',KEYSV,C'DUMP',20,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'PRINT',NBAIO,C'DUMP',40,=C'1D'                
         GOTO1 =V(CLUNPK),DMCB,KEY+2,P+1                                        
         MVC   P+7(4),KEYSV+4                                                   
         MVC   P+12(6),KEYSV+8                                                  
         EDIT  (B1,KEYSV+16),(4,P+20)                                           
         MVI   P+24,C'-'                                                        
         EDIT  (B1,KEYSV+17),(3,P+25)                                           
         GOTO1 DATCON,DMCB,(2,KEYSV+14),(5,P+30)                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P+7                                                           
CK37     EDIT  (B4,NBILGRS),(10,0(R5)),2                                        
         GOTO1 =V(PRNTBL),DMCB,=C'PRINT',NBILPRD,C'DUMP',1,=C'1D'               
         LA    R5,12(R5)                                                        
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    CK39                                                             
         AR    R4,R1                                                            
         CLI   0(R4),X'10'                                                      
         BE    CK37                                                             
CK39     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CK40     GOTO1 NBDM,DMCB,(0,=CL8'DMRSEQ'),=C'XSPDIR  ',KEY2,KEY2,0              
         B     CK10                                                             
                                                                                
CKX      MVC   P+1(20),=C'TOTAL BILL RECS READ'                                 
         EDIT  (B4,NOSTAT),(8,P+22)                                             
         MVC   P2+1(22),=C'TOTAL RECS NOT MATCHED'                              
         EDIT  (B4,NO02),(8,P2+22)                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
STATBL   DS    CL8000                                                           
*** PRINT LINE DESECT ***                                                       
         SPACE                                                                  
PLINED   DSECT                                                                  
PLNET    DS    CL4                 NETWORK                                      
         DS    CL2                                                              
PLPKCDE  DS    CL3                 PACKAGE CODE                                 
         DS    CL2                                                              
PLPKNM   DS    CL16                PACKAGE NAME                                 
         DS    CL2                                                              
PLPRGCDE DS    CL6                 PROGRAM CODE                                 
         DS    CL2                                                              
PLPRGNM  DS    CL16                PROGRAM NAME                                 
         DS    CL2                                                              
PLUNTOT  DS    CL5                 TOTAL UNITS                                  
         DS    CL2                                                              
ENDP     EQU   *-PLNET                                                          
*                                                                               
**** PASDATA STORAGE (IN W/S AREA1 FROM EDIT OVERLAY) ***                       
WORKD    DSECT                                                                  
         DS    0F                                                               
RELO     DS    F                                                                
NOSTAT   DS    F                                                                
NO02     DS    F                                                                
MYELEM   DS    CL20                                                             
*                                                                               
RSTAT    DS    CL1                 FLAG IF NURSTAT UPDATED                      
BINSTR   DS    CL2                                                              
BINEND   DS    CL2                                                              
KEYSV    DS    CL30                                                             
KEY2     DS    CL40                                                             
KEY2SV   DS    CL40                                                             
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDCD                                                       
*                                                                               
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENUBILL                                                     
       ++INCLUDE DEDEMEQUS2                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050NEMEDAA   10/01/13'                                      
         END                                                                    
