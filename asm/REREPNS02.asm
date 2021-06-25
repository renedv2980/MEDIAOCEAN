*          DATA SET REREPNS02  AT LEVEL 048 AS OF 05/01/02                      
*          DATA SET REREPNS02  AT LEVEL 046 AS OF 11/15/96                      
*          DATA SET REREPNS02  AT LEVEL 254 AS OF 10/04/96                      
*PHASE RENS02A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINTER                                                                
         TITLE 'REREPNS02 (RENS02) --- BLAIR AVAIL FIX'                         
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPNS02 --- READ DUMP TAPE AND LOAD AVAIL INFO ONTO     *            
*                      BLAIR INV RECORDS                           *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RENS02   CSECT                                                                  
         NMOD1 MYWORKX-MYWORKD,**RESW**,R9,R7,RR=R5                             
         USING MYWORKD,RC                                                       
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
SWXIT    XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         XC    COUNT,COUNT                                                      
         SPACE 1                                                                
SW20     LA    R6,REC-4            POINT TO REC-4                               
         GET   FILEIN,(R6)         (ALLOW FOR 4-BYTE HEADER)                    
         LA    R6,REC                                                           
         USING RINVREC,R6                                                       
*                                                                               
         CLI   RINVKTYP,X'12'      INVENTORY RECORD                             
         BNE   SW20                                                             
         CLC   RINVKREP,=CL2'BL'   IS AGENCY BLAIR                              
         BNE   SW20                                                             
         CLI   RINVKSRC,0          IS RECORD AN INV HEADER                      
         BNE   SW20                                                             
         OC    RINVPADY(5),RINVPADY    IS THERE AVAIL INFO                      
         BZ    SW20                                                             
         MVI   FIRSTCON,C'N'                                                    
         SPACE 1                                                                
         CLC   COUNT,=F'400'                                                    
         BH    SW40                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'TAPE',REC,C'DUMP',250,=C'1D'                  
         GOTO1 =V(PRNTBL),DMCB,=C'DYTM',RINVPADY,C'DUMP',5,=C'1D'               
SW40     OC    RINVPADY,RINVPADY                                                
         BNZ   SW42                                                             
         MVC   RINVPADY,RINVPDAY                                                
*        MVI   FIRSTCON,C'Y'                                                    
         GOTO1 =V(PRNTBL),DMCB,=C'NODAY',RINVPADY,C'DUMP',5,=C'1D'              
SW42     OC    RINVPATM,RINVPATM                                                
         BNZ   SW45                                                             
         MVC   RINVPATM,RINVPTIM                                                
*        MVI   FIRSTCON,C'Y'                                                    
         GOTO1 =V(PRNTBL),DMCB,=C'NOTIME',RINVPADY,C'DUMP',5,=C'1D'             
*                                                                               
*  READ AND UPDATE LIVE RECORD WITH OLD AVAIL INFO                              
*                                                                               
SW45     MVC   KEY(27),RINVKEY                                                  
*                                                                               
         MVC   HOLDINV,RINVKINV                                                 
         MVC   HOLDDAY,RINVPADY                                                 
         MVC   HOLDTIM,RINVPATM                                                 
         LA    R6,KEY                                                           
*                                                                               
         XC    RINVKINV,RINVKINV                                                
         MVC   RINVKDAY,HOLDINV+1                                               
         MVC   RINVKLEN,HOLDINV+2                                               
*                                                                               
         EDIT  (B1,HOLDINV),(2,HALF),FILL=0                                     
         MVC   RINVKQTR(2),HALF                                                 
*                                                                               
         BAS   RE,HIGHDIR                                                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   SW20                                                             
*                                                                               
         BAS   RE,GETSWI           GET THE RECORD                               
**************                                                                  
*        CLI   FIRSTCON,C'Y'                                                    
*        BNE   SW47                                                             
*        XC    ELEM(25),ELEM                                                    
*        MVC   ELEM(5),INVREC+12                                                
*        MVC   ELEM+8(4),INVREC+17                                              
*        MVC   MDATE,INVREC+21                                                  
*        GOTO1 DATCON,DMCB,(3,MDATE),(5,ELEM+15)                                
*        GOTO1 =V(PRNTBL),DMCB,=C'RECORD',ELEM,C'DUMP',25,=C'1D'                
*        MVI   FIRSTCON,C'N'                                                    
**************                                                                  
SW47     CLC   COUNT,=F'400'                                                    
         BH    SW50                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'REC',INVREC,C'DUMP',250,=C'1D'                
SW50     GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'04',INVREC),0               
         CLC   COUNT,=F'400'                                                    
         BH    SW51                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'DELETE',INVREC,C'DUMP',250,=C'1D'             
*                                                                               
SW51     XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING RIAPELEM,R7                                                      
         MVI   RIAPCODE,X'04'                                                   
         MVI   RIAPLEN,24                                                       
*                                                                               
         CLI   HOLDDAY,0                                                        
         BE    SW55                                                             
         GOTO1 UNDAY,DMCB,HOLDDAY,RIADAY            DAY                         
SW55     GOTO1 UNTIME,DMCB,HOLDTIM,RIATIME          TIME                        
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),INVREC,ELEM,          +        
               =C'ADD=CODE'                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'REC',ELEM,C'DUMP',24,=C'1D'                   
*                                                                               
         BAS   RE,PUTSWI                                                        
         CLC   COUNT,=F'400'                                                    
         BH    SW20                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'WRT',INVREC,C'DUMP',250,=C'1D'                
         L     RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         ST    RE,COUNT                                                         
         B     SW20                                                             
*                                                                               
SWEX     DS    0H'0'                                                            
         CLOSE FILEIN                                                           
         B     SWXIT                                                            
         EJECT                                                                  
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     SWXIT                                                            
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     SWXIT                                                            
         SPACE 3                                                                
GETSWI   LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTSWI   LA    R6,PUTREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               INVREC,(0,DMWORK)                                                
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     SWXIT                                                            
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     SWXIT                                                            
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=SWEX                                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
PURGCNT  DC    PL5'0'                                                           
RECCNT   DC    PL5'0'                                                           
         SPACE 3                                                                
         DS    0H                  HALFWORD ALIGNMENT NEEDED.                   
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL4008              AREA FOR OLD BLAIR INVENTORY RECORD          
*                                                                               
MYWORKD  DSECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
MASTREP  DS    A                                                                
SUBREP   DS    A                                                                
ABLDAREA DS    A                                                                
LBLDAREA DS    F                                                                
NUMBLD   DS    F                                                                
*********NUMBLD   EQU   (BLDLEN/LENFIXT)-1                                      
*                                                                               
HOLDINV  DS    CL3                 HOLD AREA FOR INVENTORY RECORD               
HOLDDAY  DS    CL1                 HOLD AREA FOR INVENTORY AVAIL DAY            
HOLDTIM  DS    CL4                 HOLD AREA FOR INVENTORY AVAIL TIME           
COUNT    DS    F                                                                
ELCODE   DS    CL1                                                              
MDATE    DS    CL3   (BINARY YMD MONDAY OF THIS WEEK (OR MON OF ASAT))          
FILTCODE DS    CL6                 FILTER CODE (FOR SET RECS)                   
SETTYPE  DS    X                   RECORD TYPE OF THE SET TYPE                  
FIRSTCON DS    CL1                                                              
SWICNT   DS    PL4                 SWITCH COUNT                                 
SWINTH   DS    PL4                 SWITCH DUMP (1)                              
PRGYMD   DS    CL3                 BINARY YMD - 91 DAYS PRIOR TO MDATE          
STAMATCH DS    CL1                 STATION MATCH FLAG                           
TBLCNT   DS    F                   COUNTER FOR ENTRIES IN TABLE                 
CHIND    DS    CL1                 Y=RECORD CHANGED IN THIS PASS                
DUPIND   DS    CL1                 Y=MEMBER HAS ALREADY OCCURRED IN SET         
FLAGS    DS    XL1                 X'80' = X'02' ELEMENT ENCOUNTERED            
ELEM     DS    CL256               ELEMENT WORKSPACE                            
*                                                                               
         DS    F                   LENGTH OF RECOVERY RECORD                    
*                                                                               
INVREC   DS    CL4000              AREA FOR BLAIR INVENTORY RECORD              
         EJECT                                                                  
         SPACE 2                                                                
MYWORKX  EQU   *                                                                
*                                                                               
         EJECT                                                                  
FIXD     DSECT         DSECT FOR CHANGE LIST                                    
FIXCOD   DS    0CL84                                                            
FILTER   DS    0CL35               FILTER FIELDS                                
FREP     DS    CL2                 REP                                          
FSTA     DS    CL5                 STATION                                      
FOFF     DS    CL2                 OFFICE                                       
FGRP     DS    CL2                 GROUP/SUB-GROUP                              
FTEAM    DS    CL2                 DIVISION/TEAM                                
FSAL     DS    CL3                 SALESPERSON                                  
FADV     DS    CL4                 ADVERTISER                                   
FAGY     DS    CL4                 AGENCY                                       
FAGYOF   DS    CL2                 AGENCY-OFFICE                                
FDSP     DS    CL3                 DEVELOPMENTAL SALESPERSON                    
FPTP     DS    CL3                 POINT PERSON                                 
STADATE  DS    XL3                 START DATE FILTER FOR CONTRACTS              
         SPACE 1                                                                
NEW      DS    0CL49                                                            
NSTA     DS    CL5                 NEW STATION                                  
NOFF     DS    CL2                 NEW OFFICE                                   
NGRP     DS    CL2                 NEW GROUP/SUB-GROUP                          
NTEAM    DS    CL2                 NEW DIVISION/TEAM                            
NSAL     DS    CL3                 NEW SALESPERSON                              
NADV     DS    CL4                 ADVERTISER                                   
NAGY     DS    CL4                 AGENCY                                       
NAGYOF   DS    CL2                 AGENCY-OFFICE                                
NDSP     DS    CL3                 NEW DEVELOPMENTAL SALESPERSON                
NPTP     DS    CL3                 NEW POINT PERSON                             
NEWCODE  DS    CL6                 USED FOR NEW CODE IN SWITCHING SETS          
         SPACE 1                                                                
FCSTA    DS    CL5                 COMPETING STATION                            
NCSTA    DS    CL5                 NEW COMPETING STATION                        
NCAFF    DS    CL3                 NEW COMPETING AFFILIATE                      
         SPACE 1                                                                
FIXCNT   DS    0CL216                                                           
STACHA   DS    CL1                 X'02' = CHANGE STATION RECORD                
STACNT   DS    PL4                 COUNT STATION RECORDS CHANGED                
STANTH   DS    PL4                 DUMP EVERY NTH RECORD                        
BUYCHA   DS    CL1                 X'0B' = CHANGE BUY      RECORD               
BUYCNT   DS    PL4                 COUNT                                        
BUYNTH   DS    PL4                 DUMP                                         
CONCHA   DS    CL1                 X'0C' = CHANGE CONTRACT RECORD               
CONCNT   DS    PL4                 COUNT                                        
CONNTH   DS    PL4                 DUMP                                         
INVCHA   DS    CL1                 X'12' = CHANGE INVENTORY RECORD              
INVCNT   DS    PL4                 COUNT                                        
INVNTH   DS    PL4                 DUMP                                         
BUDCHA   DS    CL1                 X'13' = CHANGE BUDGET RECORD                 
BUDCNT   DS    PL4                 COUNT                                        
BUDNTH   DS    PL4                 DUMP                                         
ATNCHA   DS    CL1                 X'27' = CHANGE ATHENA RECORD                 
ATNCNT   DS    PL4                 COUNT                                        
ATNNTH   DS    PL4                 DUMP                                         
AURCHA   DS    CL1                 X'2C' = CHANGE A.U.R. RECORD                 
AURCNT   DS    PL4                 COUNT                                        
AURNTH   DS    PL4                 DUMP                                         
ADVCHA   DS    CL1                 X'08' = CHANGE ADVERTISER RECORD             
ADVCNT   DS    PL4                 COUNT                                        
ADVNTH   DS    PL4                 DUMP                                         
PRDCHA   DS    CL1                 X'09' = CHANGE PRODUCT RECORD                
PRDCNT   DS    PL4                 COUNT                                        
PRDNTH   DS    PL4                 DUMP                                         
SDDCHA   DS    CL1                 X'26' = CHANGE SDD RECORD                    
SDDCNT   DS    PL4                 COUNT                                        
SDDNTH   DS    PL4                 DUMP                                         
AGYCHA   DS    CL1                 X'0A' = CHANGE AGENCY RECORD                 
AGYCNT   DS    PL4                 COUNT                                        
AGYNTH   DS    PL4                 DUMP                                         
SALCHA   DS    CL1                 X'06' = CHANGE SALESPERSON RECORD            
SALCNT   DS    PL4                 COUNT                                        
SALNTH   DS    PL4                 DUMP                                         
COMCHA   DS    CL1                 X'29' = COMMISSION RECORD.                   
COMCNT   DS    PL4                 COUNT                                        
COMNTH   DS    PL4                 DUMP                                         
DARCHA   DS    CL1                 X'41' = DARE RECORD.                         
DARCNT   DS    PL4                 COUNT                                        
DARNTH   DS    PL4                 DUMP                                         
MKGCHA   DS    CL1                 X'11' = MAKEGOOD RECORD                      
MKGCNT   DS    PL4                 COUNT                                        
MKGNTH   DS    PL4                 DUMP                                         
SETCHA   DS    CL1                 X'38' = SET RECORD                           
SETCNT   DS    PL4                 COUNT                                        
SETNTH   DS    PL4                 DUMP                                         
DSPCHA   DS    CL1                 X'3A' = CHANGE DEVSAL RECORD                 
DSPCNT   DS    PL4                 COUNT                                        
DSPNTH   DS    PL4                 DUMP                                         
PTPCHA   DS    CL1                 X'31' = CHANGE POINT PERSON RECORD           
PTPCNT   DS    PL4                 COUNT                                        
PTPNTH   DS    PL4                 DUMP                                         
EOPCHA   DS    CL1                 X'1B' = CHANGE EOP REC (ALL TYPES)           
EOPCNT   DS    PL4                 COUNT                                        
EOPNTH   DS    PL4                 DUMP                                         
STRCHA   DS    CL1                 X'39' = CHANGE STRATEGY REC                  
STRCNT   DS    PL4                 COUNT                                        
STRNTH   DS    PL4                 DUMP                                         
DIRCHA   DS    CL1                 X'35' = CHANGE DIRECT RESPONSE REC           
DIRCNT   DS    PL4                 COUNT                                        
DIRNTH   DS    PL4                 DUMP                                         
PRPCHA   DS    CL1                 X'43' = PROPOSAL RECORD                      
PRPCNT   DS    PL4                 COUNT                                        
PRPNTH   DS    PL4                 DUMP                                         
DUMCHA   DS    CL1                 DUMMY VARIABLES-SO DON'T UPDATE REAL         
DUMCNT   DS    PL4                 DUMMY COUNTER                                
DUMNTH   DS    PL4                 DUMMY                                        
         DS    CL1                 SPARE                                        
         DS    PL4                 SPARE                                        
         DS    PL4                 SPARE                                        
ACTION   DS    C                   P=PURGE,  C=CHANGE                           
EXCLC    DS    X                   EXECUTED COMPARE FOR AGY OR AGY/OFF          
EXMVC    DS    X                   EXECUTED MOVE FOR AGY OR AGY/OFF             
DDSTAT   DS    CL1                 FLAG: DON'T OVERWRITE STATION CALLS          
         DS    CL3                 SPARE                                        
FIXDX    EQU   *                                                                
LENFIXT  EQU   FIXDX-FIXD                                                       
         EJECT                                                                  
SWID     DSECT                                                                  
       ++INCLUDE REGENSWI                                                       
         EJECT                                                                  
*                                                                               
*  INCLUDE REGENATNA                                                            
*  INCLUDE REGENSDD                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENINVA         NEW REP RECORD                               
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048REREPNS02 05/01/02'                                      
         END                                                                    
