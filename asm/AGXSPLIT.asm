*          DATA SET AGXSPLIT   AT LEVEL 019 AS OF 07/06/20                      
*PHASE AGXSPLTA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE LOADER                                                                 
                                                                                
         TITLE 'AGXSPLIT: Split up extract file by type and pass out'           
                                                                                
***********************************************************************         
* Who  Lvl Date    Change(s)                                 Audit              
* ---- --- ------- ----------------------------------------- ----------         
* NSHE 019 04May20 Client summary record                     DSRD-26144         
* YNGX 018 10Mar20 New Timeoff for Load/Update purposes      DSRD-23410         
* YNGX 017 22JAN20 FIX EXPNESE TRANSACTION ISSUE             DSRD-25135         
* NSHE 016 06Nov19 Extend extract to include expense trans   DSRD-23387         
* MPEN 015 30May19 Fix for reading system card               ITMF-36661         
* TKLU 014 28Sep18 More MQRPT improvements                   SPEC-28142         
* TKLU 013 25Sep18 Add new KEEP= multiple cards              DSRD-19909         
*                  Fix SSB settings (for MQRPT)              SPEC-28142         
* TKLU 012 20Sep18 Don't send MQ in case of 'None'           DSRD-20170         
* TKLU 011 19Sep18 Use MQRPT not WTO for messaging (USEWTO)  DSRD-19909         
* TKLU 010 04Sep18 US merge                                  DSRD-20010         
*                                                            DSRD-20082         
* TKLU 009 13Aug18 New ANARUN=A/I to soft code dataset track DSRD-19892         
*                  allocation                                                   
* TKLU 008 01Aug18 Increase some LOADQ allocation values     ITMF-28242         
* TKLU 007 30Jul18 New CUT dimension for update purposes     DSRD-19766         
* TKLU 006 17Apr18 Country split for EU                      DSRD-17961         
* TKLU 005 09Jan18 Increase for EXP data                     ITMF-21418         
* TKLU 004 09Jan18 Increase for TRN data                     DSRD-17593         
* TKLU 003 05Jan18 Add TYPE=TRN and TYPE=EXP                 DSRD-17376         
* TKLU 002 22Nov17 Increase LARGE=L allocation for OMGGRP1   DSRD-17593         
* TKLU 001 02Nov17 BulkAPI: Split file by type (load+update) DSRD-16992         
*                                                                               
***********************************************************************         
* Sample JCL                                                          *         
* ----------                                                          *         
* TKLU.DDS.JCL(GPSPLIT*)                                              *         
*                                                                     *         
***********************************************************************         
* Description                                                         *         
* -----------                                                         *         
* This program reads in an extract file from AGXTRACT BulkAPI (in     *         
* either LOAD or UPDATE mode) and will split it into seperate files   *         
* per record ID (see AGXRECID), and finally issue an operator message *         
* for the bucket file containing the individual files.                *         
* New ANARUN=Y to run up front for track size analysis, data stored   *         
* in ANAFILE dataset and read in at 2nd time to set allocation.       *         
*                                                                     *         
***********************************************************************         
* Parameters                                                          *         
* ----------                                                          *         
* - DSPACE  - mandatory * usual DSPACE=A/C/T/Q card                   *         
*                                                                     *         
* - COUNTRY - optional  * UK/DE to set country in EU (default is UK)  *         
*                         (Ireland will also use UK)                  *         
*                                                                     *         
* - TSTRUN  - optional  * Y to set buffer dataset from TSO user, A to *         
*                         run analyse (behaves like =Y, too) - this   *         
*                         is to count numbers and sizes up front      *         
*                                                                     *         
* - ANARUN  - optional  * A to run analysis mode only (see TSTRUN=A)  *         
*                       * I to use ANAFILE for input                  *         
*                                                                     *         
* - NOMESS  - optional  * Y to skip WTO/MQ message                    *         
*                                                                     *         
* - LARGE   - optional  * Determines file sizes via valuse Y (bigger  *         
*                         files require) and L (very large files for  *         
*                         LOAD production ageencies)                  *         
*                                                                     *         
* - DSN     - optional  * override default AURADEV/AURAFQA/etc.       *         
*                         extract files                               *         
*                                                                     *         
* - SYSTEM  - mandatory * required to set Acc system in DSN           *         
*                                                                     *         
* - KEEP    - optional  * multiple cards, if 1-n AGXRECID values      *         
*                         passed only those will go out               *         
*                                                                     *         
***********************************************************************         
                                                                                
AGXSPLIT CSECT                                                                  
         PRINT NOGEN                                                            
         COPY  IEABRC                                                           
         NBASE WORKX-WORKD,**AGXS**,WORK=A(WORKC),CLEAR=YES                     
         USING WORKD,RC                                                         
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         J     MAIN                                                             
                                                                                
$$DATA   LOCTR ,                                                                
$$CODE   LOCTR ,                                                                
                                                                                
MAIN     DS    0H                                                               
         GOTOR INITIAL             Init storage and validate cards              
                                                                                
         GOTOR SETANA              Process ANAFIL                               
                                                                                
         GOTOR SETBUF              Preset DSN buffer                            
                                                                                
         GOTOR OPNEXF              Open EXFILE                                  
                                                                                
LOOP     GOTOR READEXF             Read EXFILE records                          
         JNE   END                                                              
                                                                                
         GOTOR CNVUDS              Convert 'Update/Delete' scenario             
                                                                                
         GOTOR KEEPFLT             Apply KEEP= filter                           
         JNE   LOOP                                                             
                                                                                
         GOTOR PUTREC              Put record out                               
                                                                                
         J     LOOP                                                             
                                                                                
END      GOTOR CLOEXF              Close EXFILE                                 
                                                                                
         GOTOR RESOLVE             Resolve current record type                  
                                                                                
         GOTOR CLOSE               Close and report                             
                                                                                
         XBASE RC=RETCODE,RL=1                                                  
                                                                                
***********************************************************************         
* Subroutines                                                         *         
* -----------                                                         *         
***********************************************************************         
                                                                                
***********************************************************************         
* Process ANAFIL values if applicable                                 *         
***********************************************************************         
                                                                                
SETANA   NTR1  ,                                                                
                                                                                
         CLI   ANARUN,T_INPQ       Running in input mode?                       
         JNE   SETANAX                                                          
                                                                                
         USING ANABUFD,R2                                                       
         LAY   R2,ANABUFF                                                       
         XC    ANABTYP,ANABTYP                                                  
         LHI   R4,ANABMXQ                                                       
                                                                                
         OPEN  (ANAINPF,INPUT)     Open analysis file for input                 
                                                                                
SETANA2  LA    R3,TEMP                                                          
         GET   ANAINPF,(R3)                                                     
                                                                                
         MVC   ANABTYP(ANABLNQ),TEMP                                            
         AHI   R2,ANABLNQ                                                       
         XC    ANABTYP,ANABTYP                                                  
         JCT   R4,SETANA2                                                       
         DC    H'0'                (increase ANABMXQ)                           
                                                                                
EOAIF    DS    0H                                                               
                                                                                
         CLOSE (ANAINPF)                                                        
                                                                                
SETANAX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Set DSN buffer from AGXRECID                                        *         
***********************************************************************         
                                                                                
         USING DSNBUFD,R2                                                       
SETBUF   NTR1  ,                                                                
                                                                                
         LAY   R2,DSNBUFF          Set TYpe/DSN table to naught                 
         LAY   R5,DSNBMAX                                                       
                                                                                
         LA    R3,RECTAB                                                        
                                                                                
SETBUF2  CLI   0(R3),EOTQ                                                       
         JE    SETBUFX                                                          
                                                                                
         MVC   DSNBTYP,0(R3)                                                    
                                                                                
         MVC   DSNBDSN,SPACES                                                   
         LA    R4,DSNBDSN                                                       
         LLC   R1,DSNLN                                                         
         SHI   R1,1                                                             
         MVC   0(0,R4),DSN                                                      
         EXRL  R1,*-6                                                           
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),RECTYPQ                                                    
         AHI   R4,1                                                             
         MVC   0(L'AGXCPYDQ,R4),0(R3)                                           
         AHI   R4,L'AGXCPYDQ                                                    
         MVI   0(R4),DELIMQ                                                     
         AHI   R4,1                                                             
*&&UK                                                                           
*        MVC   0(L'EUROPE,R4),EUROPE                                            
*        AHI   R4,L'EUROPE                                                      
         MVC   0(L'CTRYCODE,R4),CTRYCODE                                        
         AHI   R4,L'CTRYCODE                                                    
*&&                                                                             
*&&US*&& MVC   0(L'NORTHAM,R4),NORTHAM                                          
*&&US*&& AHI   R4,L'NORTHAM                                                     
         MVI   0(R4),DELIMQ                                                     
         AHI   R4,1                                                             
         LLC   R1,SYSLEN                                                        
         SHI   R1,1                                                             
         MVC   0(0,R4),SYSTEM                                                   
         EXRL  R1,*-6                                                           
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),DELIMQ                                                     
         AHI   R4,1                                                             
         MVI   0(R4),DATEQ                                                      
         AHI   R4,1                                                             
         MVC   0(L'CUR_DA6,R4),CUR_DA6                                          
         AHI   R4,L'CUR_DA6                                                     
         MVI   0(R4),DELIMQ                                                     
         AHI   R4,1                                                             
         MVI   0(R4),TIMEQ                                                      
         AHI   R4,1                                                             
         MVC   0(L'CUR_TI6,R4),CUR_TI6                                          
         AHI   R4,L'CUR_TI6                                                     
                                                                                
         ZAP   DSNBCNT,PZERO                                                    
         ZAP   DSNBSIZ,PZERO                                                    
                                                                                
         AHI   R2,DSNBLEN                                                       
         AHI   R3,L'AGXCPYDQ                                                    
         JCT   R5,SETBUF2                                                       
         DC    H'0'                DSNBMAX vs. # of AGXRECID entries            
                                                                                
SETBUFX  MVI   DSNBTYP,EOTQ                                                     
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Open EXFILE for this type                                           *         
***********************************************************************         
                                                                                
OPNEXF   NTR1  ,                                                                
                                                                                
         OPEN  (EXFILE,INPUT)                                                   
                                                                                
         ZAP   COUNTALL,PZERO                                                   
         ZAP   COUNTFIL,PZERO                                                   
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Close EXFILE for this type                                          *         
***********************************************************************         
                                                                                
CLOEXF   NTR1  ,                                                                
                                                                                
         CLOSE (EXFILE)                                                         
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Read for first/next EXFILE record into CUR_REC                      *         
***********************************************************************         
                                                                                
READEXF  NTR1  ,                                                                
                                                                                
         LA    R3,CUR_REC                                                       
         GET   EXFILE,(R3)                                                      
                                                                                
         AP    COUNTALL,PONE                                                    
                                                                                
         J     EXITY                                                            
                                                                                
EOEXF    DS    0H                                                               
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Convert EXFILE record type in case of Update/Delete scenario        *         
***********************************************************************         
                                                                                
CNVUDS   NTR1  ,                                                                
                                                                                
         LAY   R2,RACTAB                                                        
                                                                                
CNVUDS2  CLI   0(R2),EOTQ                                                       
         JE    CNVUDSX                                                          
         CLC   CUR_RTY,0(R2)                                                    
         JNE   CNVUDS4                                                          
         CLI   CUR_ACT,CUR_A_DQ                                                 
         JNE   CNVUDS4                                                          
         MVC   CUR_RTY,5(R2)                                                    
         J     CNVUDSX                                                          
                                                                                
CNVUDS4  AHI   R2,RACTLNQ                                                       
         J     CNVUDS2                                                          
                                                                                
CNVUDSX  DS    0H                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Apply KEP= filter(s)                                                *         
***********************************************************************         
                                                                                
KEEPFLT  NTR1  ,                                                                
                                                                                
         OC    KEEP1,KEEP1         any filter?                                  
         JZ    KEEPFY                                                           
                                                                                
         LA    R2,KEEPS                                                         
         LHI   R3,KEEP#Q                                                        
                                                                                
KEEPF2   OC    0(L'KEEP1,R2),0(R2) end of entries?                              
         JZ    KEEPFN                                                           
         CLC   CUR_RTY,0(R2)       match?                                       
         JE    KEEPFY                                                           
         AHI   R2,L'KEEP1                                                       
         JCT   R3,KEEPF2                                                        
         J     KEEPFN                                                           
                                                                                
KEEPFY   DS    0H                                                               
         J     EXITY                                                            
                                                                                
KEEPFN   DS    0H                                                               
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Write a record to type based file                                   *         
***********************************************************************         
                                                                                
         USING DSNBUFD,R2                                                       
PUTREC   NTR1  ,                                                                
                                                                                
         LAY   R2,DSNBUFF          Set Type/DSN table to naught                 
         LA    R4,T01FIL                                                        
                                                                                
PUTR02   CLI   DSNBTYP,EOTQ                                                     
         JE    *+2                 Unknown AGXRECID entry?                      
         CLC   DSNBTYP,CUR_RTY                                                  
         JE    PUTR04                                                           
         AHI   R2,DSNBLEN                                                       
         AHI   R4,T02FIL-T01FIL                                                 
         J     PUTR02                                                           
                                                                                
PUTR04   CP    DSNBCNT,PZERO       First time?                                  
         JH    PUTR50                                                           
                                                                                
         MVC   T01NAME-T01FIL(L'T01NAME,R4),DSNBDSN                             
                                                                                
         CLI   ANARUN,T_INPQ       Running in input mode                        
         JE    PUTR30                                                           
                                                                                
         LHI   RE,1                File allocation sizes (RLSE in use)          
         CLI   LARGE,L_YESQ        Note: 1/2 track = 27998 bytes                
         JNE   PUTR06              (see DDDYNALLOC for more details)            
         MHI   RE,2                                                             
                                                                                
PUTR06   CLI   LARGE,L_LOADQ                                                    
         JNE   PUTR08                                                           
         MHI   RE,4                                                             
                                                                                
PUTR08   CLC   DSNBTYP,AGXCPYDQ    Use 1,1 (2,1/4,1) tracks for:                
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXTOFFQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXCUTDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXETYDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXWCODQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXCURDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXOFFDQ                                                 
         JE    PUTR40                                                           
                                                                                
         LHI   RE,2                                                             
         CLI   LARGE,L_YESQ                                                     
         JNE   PUTR10                                                           
         MHI   RE,3                                                             
                                                                                
PUTR10   CLI   LARGE,L_LOADQ                                                    
         JNE   PUTR12                                                           
         MHI   RE,10                                                            
                                                                                
PUTR12   CLC   DSNBTYP,AGXCLIDQ    Use 2,1 (6,1/20,1) tracks for:               
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXPERDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXITMDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXCATDQ                                                 
         JE    PUTR40                                                           
                                                                                
         LHI   RE,10                                                            
         CLI   LARGE,L_YESQ                                                     
         JNE   PUTR14                                                           
         MHI   RE,5                                                             
                                                                                
PUTR14   CLI   LARGE,L_LOADQ                                                    
         JNE   PUTR16                                                           
         MHI   RE,25                                                            
                                                                                
PUTR16   CLC   DSNBTYP,AGXESTDQ    Use 10,1 (50,1/250,1) tracks for:            
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXACCDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXPRODQ                                                 
         JE    PUTR40                                                           
                                                                                
         LHI   RE,25                                                            
         CLI   LARGE,L_YESQ                                                     
         JNE   PUTR18                                                           
         MHI   RE,4                                                             
                                                                                
PUTR18   CLI   LARGE,L_LOADQ                                                    
         JNE   PUTR20                                                           
         MHI   RE,40                                                            
                                                                                
PUTR20   CLC   DSNBTYP,AGXORDDQ    Use 25,1 (100,1/1000,1) tracks for:          
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXJOBDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXEXPDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXORDSQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXESTSQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXCSMFQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXORD2Q                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXORDFQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXEST2Q                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXEXPFQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXETXDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXOTXDQ                                                 
         JE    PUTR40                                                           
                                                                                
         LHI   RE,50                                                            
         CLI   LARGE,L_YESQ                                                     
         JNE   PUTR22                                                           
         MHI   RE,10                                                            
                                                                                
PUTR22   CLI   LARGE,L_LOADQ                                                    
         JNE   PUTR24                                                           
         MHI   RE,50                                                            
                                                                                
PUTR24   CLC   DSNBTYP,AGXESTFQ    Use 50,1 (500,1/2500,1) tracks for:          
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXTIMDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXBILDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXEXPRQ                                                 
         JE    PUTR40                                                           
                                                                                
         LHI   RE,100                                                           
         CLI   LARGE,L_YESQ                                                     
         JNE   PUTR26                                                           
         MHI   RE,8                                                             
                                                                                
PUTR26   CLI   LARGE,L_LOADQ                                                    
         JNE   PUTR28                                                           
         MHI   RE,500                                                           
                                                                                
PUTR28   CLC   DSNBTYP,AGXTIMFQ    Use 100,1 (800,1/50000,1) trks for:          
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXTIMRQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXTRNDQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXTRNFQ                                                 
         JE    PUTR40                                                           
         CLC   DSNBTYP,AGXXTRFQ                                                 
         JE    PUTR40              (and any other not defined here yet)         
         J     PUTR40                                                           
                                                                                
         USING ANABUFD,R3                                                       
PUTR30   LAY   R3,ANABUFF                                                       
                                                                                
PUTR32   CLC   ANABTYP,DSNBTYP                                                  
         JE    PUTR34                                                           
         OC    ANABTYP,ANABTYP                                                  
         JZ    *+2                 (??? - see ANAOUTF/SETANA logic)             
         AHI   R3,ANABLNQ                                                       
         J     PUTR32                                                           
                                                                                
PUTR34   MVC   DUB,ANABSIZ                                                      
         CVB   RE,DUB                                                           
         DROP  R3                                                               
                                                                                
PUTR40   STCM  RE,B'0111',ALLO1AL3                                              
                                                                                
         LHI   RF,1                                                             
         STCM  RF,B'0111',ALLO2AL3                                              
                                                                                
         MVC   T01SPACE-T01FIL(L'T01SPACE,R4),ALLOCAL3                          
                                                                                
         CLI   TSTRUN,T_ANAQ       Analyse mode only?                           
         JE    PUTR42                                                           
         CLI   ANARUN,T_ANAQ                                                    
         JE    PUTR42                                                           
                                                                                
         GOTO1 VDYNALLO,DMCB,(X'80',T01SNAM-T01FIL(R4)),               +        
               (X'87',T01SPACE-T01FIL(R4)),(X'80',T01NAME-T01FIL(R4))           
                                                                                
         OPEN  ((R4),OUTPUT)       Open tape                                    
                                                                                
PUTR42   DS    0H                                                               
                                                                                
PUTR50   CLI   TSTRUN,T_ANAQ       Analyse mode only?                           
         JE    PUTR52                                                           
         CLI   ANARUN,T_ANAQ                                                    
         JE    PUTR52                                                           
                                                                                
         LA    R3,CUR_REC          Move it out                                  
         PUT   (R4),0(R3)                                                       
                                                                                
PUTR52   AP    DSNBCNT,PONE                                                     
         XR    RE,RE                                                            
         ICM   RE,B'0011',CUR_RLN                                               
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         AP    DSNBSIZ,DUB                                                      
                                                                                
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Resolve current type                                                *         
***********************************************************************         
                                                                                
         USING DSNBUFD,R2                                                       
RESOLVE  NTR1  ,                                                                
                                                                                
         LAY   R2,DSNBUFF          Set Type/DSN table to naught                 
         LA    R4,T01FIL                                                        
                                                                                
RESOLVE2 CLI   DSNBTYP,EOTQ                                                     
         JE    RESOLVEX                                                         
         CP    DSNBCNT,PZERO       Skip unused entries                          
         JE    RESOLVE4                                                         
                                                                                
         AP    COUNTFIL,PONE       Count file and close it                      
                                                                                
         CLI   TSTRUN,T_ANAQ       Analyse mode only?                           
         JE    RESOLVE4                                                         
         CLI   ANARUN,T_ANAQ                                                    
         JE    RESOLVE4                                                         
                                                                                
         CLOSE ((R4))                                                           
                                                                                
RESOLVE4 AHI   R2,DSNBLEN                                                       
         AHI   R4,T02FIL-T01FIL                                                 
         J     RESOLVE2                                                         
                                                                                
RESOLVEX DS    0H                                                               
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Close files                                                         *         
***********************************************************************         
                                                                                
CLOSE    NTR1  ,                                                                
                                                                                
         MVC   P,SPACES                                                         
         GOTOR VPRINTER                                                         
         MVC   P,SPACES                                                         
         MVC   P+1(33),=CL33'Total number of files generated:'                  
         EDITR (P8,COUNTFIL),(12,P+35),0,ZERO=NOBLANK                           
         GOTOR VPRINTER                                                         
         MVC   P,SPACES                                                         
         GOTOR VPRINTER                                                         
                                                                                
         CP    COUNTFIL,PZERO                                                   
         JE    CLOSE30                                                          
         MVI   BYTE,NOQ                                                         
                                                                                
         USING DSNBUFD,R8                                                       
         LAY   R8,DSNBUFF                                                       
         ZAP   DUBX,PZERO                                                       
                                                                                
         CLI   ANARUN,T_ANAQ                                                    
         JNE   CLOSE02                                                          
         OPEN  (ANAOUTF,OUTPUT)    Open analysis file for output                
                                                                                
CLOSE02  CLI   DSNBTYP,EOTQ                                                     
         JE    CLOSE08                                                          
         CP    DSNBCNT,PZERO       Skip unused entries                          
         JE    CLOSE06                                                          
         CLI   BYTE,YESQ                                                        
         JE    CLOSE04                                                          
         MVI   BYTE,YESQ                                                        
         MVC   P,SPACES                                                         
         MVC   P+1(5),=C'Type:'                                                 
         MVC   P+7(6),=C'Lines:'                                                
         MVC   P+18(5),=C'Size:'                                                
         MVC   P+33(7),=C'Tracks:'                                              
         MVC   P+42(4),=C'DSN:'                                                 
         GOTOR VPRINTER                                                         
                                                                                
CLOSE04  MVC   P,SPACES                                                         
         MVC   P+1(L'DSNBTYP),DSNBTYP                                           
         EDITR (P6,DSNBCNT),(10,P+7),0,ZERO=NOBLANK                             
         EDITR (P10,DSNBSIZ),(14,P+18),0,ZERO=NOBLANK                           
         ZAP   PL16,DSNBSIZ                                                     
         SRP   PL16,64-3,0                                                      
         ZAP   DUB2,PTRACK         (approx. size of track in Kb)                
         DP    PL16,DUB2                                                        
         AP    PL16(8),PONE                                                     
         EDITR (P8,PL16),(8,P+33),0,ZERO=NOBLANK                                
         MVC   P+42(3),=C'N/A'                                                  
         CLI   ANARUN,T_ANAQ                                                    
         JE    CLOSE05                                                          
         MVC   P+42(L'DSNBDSN),DSNBDSN                                          
                                                                                
CLOSE05  GOTOR VPRINTER                                                         
         AP    DUBX,DSNBCNT                                                     
                                                                                
         CLI   ANARUN,T_ANAQ                                                    
         JNE   CLOSE06                                                          
                                                                                
         USING ANABUFD,R2                                                       
         LA    R2,TEMP                                                          
         MVC   TEMP,SPACES                                                      
         AP    PL16(8),PONE        (one more extra)                             
         MVC   ANABTYP,DSNBTYP                                                  
         ZAP   ANABSIZ,PL16(8)     (edit out for visibility)                    
         EDITR (P8,PL16),(8,ANABSIZ+L'ANABSIZ+5),0,ZERO=NOBLANK                 
                                                                                
         PUT   ANAOUTF,TEMP                                                     
                                                                                
CLOSE06  AHI   R8,DSNBLEN                                                       
         J     CLOSE02                                                          
                                                                                
CLOSE08  CLI   ANARUN,T_ANAQ                                                    
         JNE   CLOSE10                                                          
                                                                                
         CLOSE (ANAOUTF)                                                        
                                                                                
CLOSE10  LAY   R8,DSNBUFF                                                       
                                                                                
         MVC   P,SPACES                                                         
         GOTOR VPRINTER                                                         
         MVC   P,SPACES                                                         
         MVC   P+1(33),=CL33'Control total extracted records:'                  
         EDITR (P8,DUBX),(12,P+35),0,ZERO=NOBLANK                               
         GOTOR VPRINTER                                                         
                                                                                
         CLI   ANARUN,T_ANAQ       Analyse mode only?                           
         JE    CLOSE26                                                          
                                                                                
         LA    R2,DUBX                                                          
         EXTRACT (2),'S',FIELDS=TIOT                                            
         L     R2,DUBX                                                          
         MVC   DSNNAMO(4),0(R2)                                                 
                                                                                
         LA    RE,DSNNAMO          Default                                      
         LA    R1,L'DSNNAMO-1                                                   
                                                                                
         CLI   TSTRUN,T_YESQ       Local override for testing                   
         JE    CLOSE12                                                          
         CLI   TSTRUN,T_ANAQ                                                    
         JE    CLOSE12                                                          
         LA    RE,DSNNAMA                                                       
         LA    R1,L'DSNNAMA-1                                                   
         CLI   DSPACE,C'A'                                                      
         JE    CLOSE12                                                          
         LA    R1,L'DSNNAMC-1                                                   
         LA    RE,DSNNAMC                                                       
         CLI   DSPACE,C'C'                                                      
         JE    CLOSE12                                                          
         LA    R1,L'DSNNAMT-1                                                   
         LA    RE,DSNNAMT                                                       
         CLI   DSPACE,C'T'                                                      
         JE    CLOSE12                                                          
         LA    R1,L'DSNNAMQ-1                                                   
         LA    RE,DSNNAMQ                                                       
         CLI   DSPACE,C'Q'                                                      
         JNE   *+2                                                              
                                                                                
CLOSE12  MVC   DSNNAME(0),0(RE)                                                 
         EXRL  R1,*-6                                                           
         LA    R3,DSNNAME+1(R1)                                                 
         MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
                                                                                
*&&UK                                                                           
*        MVC   0(L'EUROPE,R3),EUROPE                                            
*        AHI   R3,L'EUROPE                                                      
         MVC   0(L'CTRYCODE,R3),CTRYCODE                                        
         AHI   R3,L'CTRYCODE                                                    
*&&                                                                             
*&&US*&& MVC   0(L'NORTHAM,R3),NORTHAM                                          
*&&US*&& AHI   R3,L'NORTHAM                                                     
         MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
                                                                                
         LLC   R1,SYSLEN                                                        
         SHI   R1,1                                                             
         MVC   0(0,R3),SYSTEM                                                   
         EXRL  R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
                                                                                
         MVI   0(R3),DATEQ                                                      
         AHI   R3,1                                                             
         MVC   0(L'CUR_DA6,R3),CUR_DA6                                          
         AHI   R3,L'CUR_DA6                                                     
         MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
         MVI   0(R3),TIMEQ                                                      
         AHI   R3,1                                                             
         MVC   0(L'CUR_TI6,R3),CUR_TI6                                          
         AHI   R3,L'CUR_TI6                                                     
                                                                                
         MVC   P,SPACES                                                         
         GOTOR VPRINTER                                                         
         MVC   P,SPACES                                                         
         MVC   P+1(7),=C'Bucket:'                                               
         MVC   P+10(L'DSNNAME),DSNNAME                                          
         GOTOR VPRINTER                                                         
                                                                                
         GOTO1 VDYNALLO,DMCB,(X'80',D2NAME),(X'87',DSNSPACE),          +        
               (X'80',DSNNAME)                                                  
                                                                                
         OPEN  (DSNFILE,OUTPUT)    Open bucket                                  
                                                                                
CLOSE14  CLI   DSNBTYP,EOTQ                                                     
         JE    CLOSE20                                                          
         CP    DSNBCNT,PZERO       Skip unused entries                          
         JE    CLOSE16                                                          
                                                                                
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(L'DSNBDSN),DSNBDSN                                          
         PUT   DSNFILE,TEMP                                                     
                                                                                
CLOSE16  AHI   R8,DSNBLEN                                                       
         J     CLOSE14                                                          
         DROP  R8                                                               
                                                                                
CLOSE20  CLOSE (DSNFILE)                                                        
                                                                                
         MVI   RETCODE,2                                                        
         CLI   NOMESS,YESQ                                                      
         JE    CLOSEX                                                           
         CLI   USEWTO,YESQ                                                      
         JNE   CLOSE22             Message out via WTO                          
                                                                                
         MVC   OFILKEY(L'DSNNAME),DSNNAME                                       
         XR    R0,R0                                                            
         WTO   TEXT=OFILL,MCSFLAG=HRDCPY                                        
         J     CLOSE26                                                          
                                                                                
CLOSE22  DS    0H                  Message out via MQ                           
                                                                                
         MVI   BYTE,X'80'+X'20'    (X'01')                                      
         GOTOR VMQRPT,DMCB,MQOPEN,MYMQHDR,(BYTE,0)                              
         CLI   8(R1),0             All okay ?                                   
         JNE   *+2                                                              
                                                                                
         MVC   MYFILE,DSNNAME                                                   
         LHI   RF,MYMSGLQ                                                       
         GOTOR VMQRPT,DMCB,MQPUT,MYRECTY,(RF)                                   
         CLI   8(R1),0             All okay ?                                   
         JNE   *+2                                                              
                                                                                
         GOTOR VMQRPT,DMCB,MQCLOSE                                              
         CLI   8(R1),0             All okay ?                                   
         JNE   *+2                                                              
                                                                                
         MVC   P,SPACES                                                         
         GOTOR VPRINTER                                                         
         MVC   P+1(40),=CL40'* MQ message OPEN/PUT/CLOSE completed *'           
         GOTOR VPRINTER                                                         
         MVC   P+10(120),MYMQMSG                                                
         GOTOR VPRINTER                                                         
         MVC   P,SPACES                                                         
         GOTOR VPRINTER                                                         
                                                                                
CLOSE26  MVI   RETCODE,1                                                        
                                                                                
         J     CLOSEX                                                           
                                                                                
CLOSE30  MVC   P,SPACES                                                         
         MVC   P+1(30),=CL30'*** No datasets produced ***'                      
         GOTOR VPRINTER                                                         
         MVC   P,SPACES                                                         
         GOTOR VPRINTER                                                         
                                                                                
         CLI   ANARUN,T_ANAQ       Analyse mode only?                           
         JE    CLOSE36                                                          
                                                                                
         MVI   RETCODE,4                                                        
         CLI   NOMESS,YESQ                                                      
         JE    CLOSEX                                                           
         CLI   USEWTO,YESQ                                                      
         JNE   CLOSE32             Message out via WTO                          
                                                                                
         MVC   OFILKEY(L'NOFILSQ),NOFILSQ                                       
         XR    R0,R0                                                            
         WTO   TEXT=OFILL,MCSFLAG=HRDCPY                                        
         J     CLOSE36                                                          
                                                                                
CLOSE32  DS    0H                  Message out via MQ: If 'None'                
*                                  skip MQ message completely|                  
                                                                                
*        MVC   MYPREF,NOFILSQ                                                   
*        MVI   BYTE,X'80'+X'20'    (X'01')                                      
*        GOTOR VMQRPT,DMCB,MQOPEN,MYMQHDR,(BYTE,0)                              
*        CLI   8(R1),0             All okay ?                                   
*        JNE   *+2                                                              
                                                                                
*        MVC   MYFILE,SPACES                                                    
*        LHI   RF,MYMSGLQ                                                       
*        GOTOR VMQRPT,DMCB,MQPUT,MYRECTY,(RF)                                   
*        CLI   8(R1),0             All okay ?                                   
*        JNE   *+2                                                              
                                                                                
*        GOTOR VMQRPT,DMCB,MQCLOSE                                              
*        CLI   8(R1),0             All okay ?                                   
*        JNE   *+2                                                              
                                                                                
CLOSE36  MVI   RETCODE,3                                                        
                                                                                
CLOSEX   DS    0H                                                               
         J     EXIT                                                             
                                                                                
*********************************************************************           
* General exits                                                     *           
*********************************************************************           
                                                                                
EXITL    DS    0H                  Low                                          
EXITN    LHI   R0,0                Not Equal                                    
         J     EXITCC                                                           
EXITY    LHI   R0,1                Equal                                        
         J     EXITCC                                                           
EXITH    LHI   R0,2                High                                         
                                                                                
EXITCC   CHI   R0,1                Set condition code                           
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
*********************************************************************           
* Initialisation routine.                                           *           
* 1. Set up working storage.                                        *           
* 2. Process SYSIN cards.                                           *           
*********************************************************************           
                                                                                
INITIAL  NTR1  ,                                                                
                                                                                
* Set up External routines                                                      
         MVC   VCARDS,=V(CARDS)                                                 
         MVC   VDATCON,=V(DATCON)                                               
         MVC   VDATVAL,=V(DATVAL)                                               
         MVC   VHEXIN,=V(HEXIN)                                                 
         MVC   VHEXOUT,=V(HEXOUT)                                               
         MVC   VLOGIO,=V(LOGIO)                                                 
         MVC   VPRINTER,=V(PRINTER)                                             
         MVC   VPRNTBL,=V(PRNTBL)                                               
         MVC   VNUMVAL,=V(NUMVAL)                                               
         MVC   VLOADER,=V(LOADER)                                               
         MVC   VDYNALLO,=V(DYNALLOC)                                            
         MVC   VMQRPT,=V(MQRPT)                                                 
         MVC   VUTL,=V(UTL)                                                     
         MVC   ASSB,=A(SSB)                                                     
                                                                                
* Set up working storage                                                        
         MVI   SPACES,SPACEQ       Preset SPACES to space                       
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
                                                                                
         XC    TRTAB,TRTAB                                                      
         MVI   TRTAB+C'>',X'24'                                                 
         MVI   TRTAB+C'<',X'44'                                                 
         MVI   TRTAB+C'^',X'74'                                                 
         MVI   TRTAB+C'=',X'84'                                                 
                                                                                
         TIME                                                                   
         LTR   R0,R0                                                            
         JZ    *+2                 BAD RETURN FROM MACRO                        
         ST    R0,FULL                                                          
         ST    R1,FULL2                                                         
                                                                                
         L     R1,FULL                                                          
         SRL   R1,28                                                            
         STC   R1,CUR_TIM                                                       
         OI    CUR_TIM,X'F0'                                                    
         L     R1,FULL                                                          
         SRL   R1,24                                                            
         STC   R1,CUR_TIM+1                                                     
         OI    CUR_TIM+1,X'F0'                                                  
         L     R1,FULL                                                          
         SRL   R1,20                                                            
         STC   R1,CUR_TIM+2                                                     
         OI    CUR_TIM+2,X'F0'                                                  
         L     R1,FULL                                                          
         SRL   R1,16                                                            
         STC   R1,CUR_TIM+3                                                     
         OI    CUR_TIM+3,X'F0'                                                  
         L     R1,FULL                                                          
         SRL   R1,12                                                            
         STC   R1,CUR_TIM+4                                                     
         OI    CUR_TIM+4,X'F0'                                                  
         L     R1,FULL                                                          
         SRL   R1,8                                                             
         STC   R1,CUR_TIM+5                                                     
         OI    CUR_TIM+5,X'F0'                                                  
         L     R1,FULL                                                          
         SRL   R1,4                                                             
         STC   R1,CUR_TIM+6                                                     
         OI    CUR_TIM+6,X'F0'                                                  
         L     R1,FULL                                                          
         STC   R1,CUR_TIM+7                                                     
         OI    CUR_TIM+7,X'F0'                                                  
                                                                                
         GOTO1 VDATCON,DMCB,(6,FULL2),(20,CUR_DAT)                              
                                                                                
         LAY   R7,AREA             A(work area for control cards)               
                                                                                
* Read in cards                                                                 
VALC002  GOTOR VCARDS,DMCB,(R7),=C'RE00'                                        
         CLC   =C'/*',0(R7)                                                     
         JE    VALC020                                                          
         LR    R3,R7               A(start of card)                             
         CLI   0(R3),C'*'          * in col 1 is a comment                      
         JE    VALC014             ignore comment cards                         
         AHI   R1,79                                                            
         MVC   P(80),0(R7)                                                      
         GOTOR VPRINTER            Print out card                               
                                                                                
* Scan CARDTAB for card                                                         
         USING CARDTABD,R4                                                      
         LA    R4,CARDTAB                                                       
                                                                                
VALC004  DS    0H                                                               
         CLI   0(R4),EOTQ                                                       
         JE    CARDER2             Error if card not found                      
         LLC   R1,CARDKXLN                                                      
         CLC   0(0,R3),CARDKEY                                                  
         EXRL  R1,*-6                                                           
         JE    VALC006                                                          
         AHI   R4,CARDTBLQ         Try next entry in CARDTAB                    
         J     VALC004                                                          
                                                                                
* Register usage:                                                               
*   R2 - branch condition (set by TRT)                                          
*   R3 - points to the delimeter                                                
*   R7 - points to start of card                                                
VALC006  DS    0H                                                               
         LA    R3,1(R1,R3)         Point to delimiter                           
         XR    RF,RF                                                            
         TM    CARDIND,CARDNUMQ    Numbers before delimeter ?                   
         JNO   VALC008             No                                           
         LA    RF,79(R7)           A(end of card)                               
         SR    RF,R3               L' to check                                  
                                                                                
VALC008  TRT   0(0,R3),TRTAB       Is this a valid delimeter ?                  
         EXRL  RF,*-6                                                           
         JZ    CARDER2             No, error                                    
         ICM   RF,15,CARDVDSP                                                   
         TM    CARDIND,CARDRTNQ    Validation routine ?                         
         JNO   VALC010             No                                           
         BASR  RE,RF               Yes, call validation routine                 
         JNE   CARDER2                                                          
         J     VALC012                                                          
                                                                                
* Move value into local storage                                                 
VALC010  IC    R1,CARDVXLN         Get len for move                             
         LA    RF,WORKD(RF)                                                     
         MVC   0(0,RF),1(R3)       Move to output area                          
         EXRL  R1,*-6                                                           
                                                                                
VALC012  MVI   ANYCARDS,YESQ                                                    
                                                                                
VALC014  DS    0H                                                               
         J     VALC002                                                          
                                                                                
* All cards have been processed. Now validate cards in context.                 
                                                                                
VALC020  DS    0H                                                               
         CLI   ANYCARDS,YESQ                                                    
         JNE   CARDER1                                                          
                                                                                
* Validate DSPACE                                                               
         CLI   DSPACE,0                                                         
         JE    CARDER3             Must provide DSPACE=                         
         CLI   DSPACE,C'T'                                                      
         JE    VALC022                                                          
         CLI   DSPACE,C'A'                                                      
         JE    VALC022                                                          
         CLI   DSPACE,C'C'                                                      
         JE    VALC022                                                          
         CLI   DSPACE,C'Q'                                                      
         JE    VALC022                                                          
         CLI   DSPACE,C'B'                                                      
         JNE   CARDER4                                                          
                                                                                
         USING SSOOFF,RF                                                        
VALC022  L     RF,ASSB                                                          
         MVC   SSODSPAC,DSPACE     Set dataspace ID                             
         MVI   SSOSTAT2,SSOSGALO+SSOSNRCV                                       
         DROP  RF                                                               
                                                                                
* Validate TSTRUN                                                               
         CLI   TSTRUN,0                                                         
         JE    VALC024                                                          
         CLI   TSTRUN,T_ANAQ                                                    
         JE    VALC024                                                          
         CLI   TSTRUN,T_YESQ                                                    
         JNE   CARDER9                                                          
                                                                                
* Validate SYSTEM                                                               
VALC024  CLI   SYSLEN,0                                                         
         JE    CARDER11                                                         
                                                                                
* Validate ANARUN                                                               
         CLI   ANARUN,0                                                         
         JE    VALC026                                                          
         CLI   ANARUN,T_ANAQ                                                    
         JE    VALC026                                                          
         CLI   ANARUN,T_INPQ                                                    
         JNE   CARDER13                                                         
                                                                                
* Validate DSN                                                                  
VALC026  CLI   DSNLN,0                                                          
         JH    VALC030                                                          
         LA    RE,DSNNAMA                                                       
         LHI   RF,L'DSNNAMA+1                                                   
         CLI   DSPACE,C'A'                                                      
         JE    VALC028                                                          
         LA    RE,DSNNAMQ                                                       
         LHI   RF,L'DSNNAMQ+1                                                   
         CLI   DSPACE,C'Q'                                                      
         JE    VALC028                                                          
         LA    RE,DSNNAMC                                                       
         LHI   RF,L'DSNNAMC+1                                                   
         CLI   DSPACE,C'C'                                                      
         JE    VALC028                                                          
         LA    RE,DSNNAMT                                                       
         LHI   RF,L'DSNNAMT+1                                                   
         CLI   DSPACE,C'T'                                                      
         JNE   *+2                                                              
                                                                                
VALC028  MVC   DSN(7),0(RE)                                                     
         MVI   DSN+7,DELIMQ                                                     
         MVI   DSNLN,L'DSNNAMA+1                                                
                                                                                
* Validate COUNTRY                                                              
VALC030  DS    0H                                                               
*&&UK*&& CLC   CTRYCODE,SPACES                                                  
*&&UK*&& JH    VALC032                                                          
*&&UK*&& MVC   CTRYCODE,CTRY_UK    set default                                  
                                                                                
VALC032  DS    0H                                                               
         XIT1  ,                                                                
                                                                                
* Invalid control card. Print it out and abend.                                 
CARDER1  MVC   P(20),=CL20'No control cards'                                    
         LA    R2,501                                                           
         J     VALCDIE                                                          
                                                                                
CARDER2  MVC   P(20),=CL20'Invalid control card'                                
         LA    R2,502                                                           
         J     VALCDIE                                                          
                                                                                
CARDER3  MVC   P(18),=CL20'DSPACE= is missing'                                  
         LA    R2,503                                                           
         J     VALCDIE                                                          
                                                                                
CARDER4  MVC   P(20),=CL20'DSPACE= is invalid'                                  
         LA    R2,504                                                           
         J     VALCDIE                                                          
                                                                                
CARDER5  MVC   P(28),=CL28'DSN= value too long'                                 
         LA    R2,505                                                           
         J     VALCDIE                                                          
                                                                                
CARDER6  MVC   P(28),=CL28'DSN= value too short'                                
         LA    R2,506                                                           
         J     VALCDIE                                                          
                                                                                
CARDER7  MVC   P(28),=CL28'DSN= value is numeric'                               
         LA    R2,507                                                           
         J     VALCDIE                                                          
                                                                                
CARDER8  MVC   P(28),=CL28'DSN= value is invalid'                               
         LA    R2,508                                                           
         J     VALCDIE                                                          
                                                                                
CARDER9  MVC   P(28),=CL28'Invalid TSTRUN= value'                               
         LA    R2,509                                                           
         J     VALCDIE                                                          
                                                                                
CARDER10 MVC   P(28),=CL28'SYSTEM= card invalid'                                
         LA    R2,510                                                           
         J     VALCDIE                                                          
                                                                                
CARDER11 MVC   P(28),=CL28'SYSTEM= card missing'                                
         LA    R2,511                                                           
         J     VALCDIE                                                          
                                                                                
CARDER12 MVC   P(28),=CL28'COUNTRY= value invalid'                              
         LA    R2,512                                                           
         J     VALCDIE                                                          
                                                                                
CARDER13 MVC   P(28),=CL28'ANARUN= value invalid'                               
         LA    R2,513                                                           
         J     VALCDIE                                                          
                                                                                
CARDER14 MVC   P(28),=CL28'Invalid KEEP= RecID'                                 
         LA    R2,514                                                           
         J     VALCDIE                                                          
                                                                                
CARDER15 MVC   P(28),=CL28'Too many KEEP= cards'                                
         LA    R2,515                                                           
         J     VALCDIE                                                          
                                                                                
CARDER16 MVC   P(28),=CL28'Duplicate KEEP= card'                                
         LA    R2,516                                                           
         J     VALCDIE                                                          
                                                                                
VALCDIE  DS    0H                                                               
         GOTOR VPRINTER            Print out card                               
                                                                                
VALCDIE2 ABEND (R2),DUMP                                                        
                                                                                
*********************************************************************           
* DSN=anananan                                                      *           
*********************************************************************           
                                                                                
DSNVAL   NTR1  ,                                                                
         LA    R2,79(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,3(R7)            A(delimeter)                                 
DSNVAL2  CLI   0(R2),C' '                                                       
         JH    DSNVAL4                                                          
         BRXH  R2,RE,DSNVAL2                                                    
         J     CARDERR                                                          
                                                                                
DSNVAL4  LR    R5,R2               Found end of string                          
         SR    R2,RF                                                            
         CHI   R2,L'DSN                                                         
         JH    CARDER5                                                          
         CHI   R2,4                                                             
         JL    CARDER6                                                          
         CLI   4(R7),C'0'                                                       
         JH    CARDER7                                                          
         CLI   4(R7),C'A'                                                       
         JL    CARDER8                                                          
                                                                                
         CLI   1(R5),DELIMQ                                                     
         JE    DSNVAL6                                                          
         MVI   1(R5),DELIMQ                                                     
         AHI   R2,1                                                             
                                                                                
DSNVAL6  STC   R2,DSNLN                                                         
         SHI   R2,1                                                             
         MVC   DSN(0),4(R7)                                                     
         EXRL  R2,*-6                                                           
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* KEEP=nnnnn multiple cards                                         *           
*********************************************************************           
                                                                                
KEEPVAL  NTR1  ,                                                                
                                                                                
         LA    RF,5(R7)            point to RecID                               
         LA    RE,AGXCPYDQ                                                      
                                                                                
KEEPV2   CLC   0(5,RE),0(RF)       match?                                       
         JE    KEEPV4                                                           
         CLI   0(RE),0             eot?                                         
         JE    CARDER14                                                         
         AHI   RE,L'AGXCPYDQ                                                    
         J     KEEPV2                                                           
                                                                                
KEEPV4   CLC   5(5,RF),SPACES      Ensure 5 bytes ...                           
         JH    CARDER14                                                         
                                                                                
         LA    R2,KEEP1            Put to storage                               
         LHI   R3,KEEP#Q                                                        
                                                                                
KEEPV6   OC    0(L'KEEP1,R2),0(R2) spare?                                       
         JZ    KEEPV8                                                           
         CLC   0(L'KEEP1,R2),0(RF) duplicate?                                   
         JE    CARDER16                                                         
         AHI   R2,L'KEEP1                                                       
         JCT   R3,KEEPV6                                                        
         J     CARDER15                                                         
                                                                                
KEEPV8   MVC   0(L'KEEP1,R2),0(RF)                                              
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* COUNTRY=aa                                                        *           
*********************************************************************           
                                                                                
CTRVAL   NTR1  ,                                                                
         LA    R2,79(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,7(R7)            A(delimeter)                                 
CTRVAL2  CLI   0(R2),C' '                                                       
         JH    CTRVAL4                                                          
         BRXH  R2,RE,CTRVAL2                                                    
         J     CARDERR                                                          
                                                                                
CTRVAL4  LR    R5,R2               Found end of string                          
         SR    R2,RF                                                            
         CHI   R2,L'CTRYCODE                                                    
         JH    CARDER12                                                         
*&&US                                                                           
         CLC   8(2,R7),NORTHAM                                                  
         JE    CTRVAL6                                                          
*&&                                                                             
         CLC   8(2,R7),CTRY_DE                                                  
         JE    CTRVAL6                                                          
         CLC   8(2,R7),CTRY_UK                                                  
***      JE    CTRVAL6                                                          
***      CLC   8(2,R7),CTRY_IE                                                  
         JNE   CARDER12                                                         
                                                                                
CTRVAL6  MVC   CTRYCODE,8(R7)      Save country for DSN                         
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* DDSIO=  validation                                                *           
*********************************************************************           
                                                                                
DSIOVAL  NTR1  ,                                                                
         LA    R2,6(R7)                                                         
         L     RF,=V(DDSIO)        Set up DDSIO override                        
         MVC   0(8,RF),0(R2)                                                    
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* SYSTEM= validation                                                *           
*********************************************************************           
                                                                                
SYSVAL   NTR1  ,                                                                
         LA    R2,20(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,6(R7)            A(delimeter)                                 
SYSVAL2  CLI   0(R2),C' '                                                       
         JH    SYSVAL4                                                          
         BRXH  R2,RE,SYSVAL2                                                    
         J     CARDERR                                                          
                                                                                
SYSVAL4  LR    R5,R2               Found end of string                          
         SR    R2,RF                                                            
         CHI   R2,L'SYSTEM                                                      
         JH    CARDER10                                                         
                                                                                
         STC   R2,SYSLEN                                                        
         SHI   R2,1                                                             
         MVC   SYSTEM(0),7(R7)                                                  
         EXRL  R2,*-6                                                           
         J     CARDOK                                                           
                                                                                
* Exit from card validation routines                                            
                                                                                
CARDOK   CR    RB,RB               Set r/c EQ                                   
         J     *+6                                                              
                                                                                
CARDERR  LTR   RB,RB               Set r/c NE                                   
         J     VALCARDX                                                         
                                                                                
VALCARDX XIT1  ,                                                                
                                                                                
*********************************************************************           
* Global literals and constants (addressed by RB)                   *           
*********************************************************************           
                                                                                
$$DATA   LOCTR ,                                                                
GLOBALS  DS    0D                                                               
                                                                                
         LTORG ,                                                                
                                                                                
PONE     DC    PL1'1'                                                           
PZERO    DC    PL1'0'                                                           
PTRACK   DC    PL2'52'                                                          
                                                                                
NOFILSQ  DC    CL10'None'                                                       
EUROPE   DC    CL2'EU'                                                          
NORTHAM  DC    CL2'NA'                                                          
CTRY_DE  DC    CL2'DE'                                                          
CTRY_UK  DC    CL2'UK'                                                          
***CTRY_IE  DC    CL2'IE'                                                       
                                                                                
MQOPEN   DC    CL6'OPEN'                                                        
MQPUT    DC    CL6'PUT'                                                         
MQCLOSE  DC    CL6'CLOSE'                                                       
                                                                                
MQ_CSC   DS    CL25'MO.AURAWH.NOTIFY.CSC1.AQ'       FLASH                       
MQ_TST   DC    CL25'MO.AURAWH.NOTIFY.DEV1.AQ'       TEST/DEV                    
MQ_FQA   DC    CL25'MO.AURAWH.NOTIFY.QA1.AQ'        QA                          
MQ_ADV   DC    CL25'MO.AURAWH.NOTIFY.PRD1.AQ'       PROD                        
                                                                                
* MQ MQRPT message                                                              
                                                                                
MYMQLNQ  EQU   200                                                              
MYMQMSG  DC    (MYMQLNQ)C' '       200 byte space filled message                
                                                                                
* Header                                                                        
         ORG   MYMQMSG+001                                                      
MYMQHDR  DS    0CL16               16 byte message routing label                
         DC    16C'*'                                                           
         ORG   MYMQHDR+001                                                      
MYROUTE  DC    C'AURAWH'           AURA Warehouse Q routing                     
                                                                                
* Message                                                                       
         ORG   MYMQHDR+017                                                      
MYRECTY  DC    C'AURAWH-EXTRACT'   Record type                                  
         ORG   MYMQHDR+60                                                       
MYPREF   DC    C'AURA-DSN='        Prefix                                       
         ORG   MYMQHDR+60+9                                                     
MYFILE   DC    CL(L'DSNNAME)' '    MVS DSN                                      
         ORG   ,                                                                
MYMSGLQ  EQU   *-MYRECTY           L' message                                   
                                                                                
OFIL2    DC    AL2(40)                                                          
         DC    CL40'No files have been generated'                               
                                                                                
OFILH    DC    AL2(40)                                                          
         DC    CL40'AURA Extract Dataset Name'                                  
                                                                                
OFILL    DC    AL2(90)                                                          
OFILM    DC    CL90' '                                                          
         ORG   OFILM                                                            
         DC    C'AURA-DSN='                                                     
OFILKEY  DC    CL81' '                                                          
         ORG   OFILM+L'OFILM                                                    
                                                                                
DSNNAMA  DC    CL7'AURAPRD'                                                     
DSNNAMQ  DC    CL7'AURAFQA'                                                     
DSNNAMC  DC    CL7'AURACSC'                                                     
DSNNAMT  DC    CL7'AURADEV'                                                     
*&&UK                                                                           
DSNNAMO  DC    CL9'YNGX.AURA'      LOCAL OVERRIDE FOR TESTING                   
*&&                                                                             
*&&US                                                                           
DSNNAMO  DC    CL9'VGUP.AURA'      LOCAL OVERRIDE FOR TESTING                   
*&&                                                                             
                                                                                
* AGXRECID                                                                      
RECTAB   DS    0H                                                               
       ++INCLUDE AGXRECID                                                       
RECTABX  DC    X'00'                                                            
                                                                                
RACTAB   DS    0H                  Record/action conversion table               
         DC  CL5'05311',CL5'05331' . AGXTRNUQ: JobTransactionFacts              
RACTLNQ  EQU   *-RACTAB            Keep in sync with DCs in AGXRECID!           
         DC  CL5'05337',CL5'05338' . AGXXTRUQ: ExpTransactionFacts              
         DC  CL5'05313',CL5'05334' . AGXORDUG: OrderFacts                       
         DC  CL5'05315',CL5'05333' . AGXESTUG: EstimateFacts                    
         DC  CL5'05317',CL5'05332' . AGXEXPUG: ExpenseClaimFacts                
         DC  CL5'05319',CL5'05335' . AGXTIMUG: TimeSheetFacts                   
         DC    AL1(EOTQ)           (not easy to include the equs here)          
                                                                                
*********************************************************************           
* Datasets                                                          *           
*********************************************************************           
                                                                                
EXFILE   DCB   DDNAME=EXFILE,                                          +        
               DSORG=PS,                                               +        
               MACRF=(GM),                                             +        
               RECFM=VB,                                               +        
               LRECL=8192,                                             +        
               EODAD=EOEXF                                                      
                                                                                
DSNFILE  DCB   DDNAME=DSNFIL,                                          +        
               DSORG=PS,                                               +        
               MACRF=PM,                                               +        
               RECFM=FB,                                               +        
               LRECL=96,                                               +        
               BLKSIZE=4800                                                     
                                                                                
ANAOUTF  DCB   DDNAME=ANAFIL,                                          +        
               DSORG=PS,                                               +        
               MACRF=PM,                                               +        
               RECFM=FB,                                               +        
               LRECL=96,                                               +        
               BLKSIZE=4800                                                     
                                                                                
ANAINPF  DCB   DDNAME=ANAFIL,                                          +        
               DSORG=PS,                                               +        
               MACRF=GM,                                               +        
               RECFM=FB,                                               +        
               LRECL=96,                                               +        
               BLKSIZE=4800,                                           +        
               EODAD=EOAIF                                                      
                                                                                
D2NAME   EQU   DSNFILE+40         DDNAME is at DCB+40                           
DSNSPACE DC    AL3(2,2)           TRK,(1,1) should be enough                    
DSNNAME  DC    CL44' '                                                          
                                                                                
CARDTAB  DS    0H                                                               
         DC    C'ANARUN    ',AL1(5),AL1(0),AL1(0),AL4(ANARUN-WORKD)             
         DC    C'COUNTRY   ',AL1(6),AL1(0),AL1(CARDRTNQ),AL4(CTRVAL)            
         DC    C'DDSIO     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(DSIOVAL)           
         DC    C'DSPACE    ',AL1(5),AL1(0),AL1(0),AL4(DSPACE-WORKD)             
         DC    C'DSN       ',AL1(2),AL1(0),AL1(CARDRTNQ),AL4(DSNVAL)            
         DC    C'KEEP      ',AL1(3),AL1(0),AL1(CARDRTNQ),AL4(KEEPVAL)           
         DC    C'LARGE     ',AL1(4),AL1(0),AL1(0),AL4(LARGE-WORKD)              
         DC    C'NOMESS    ',AL1(5),AL1(0),AL1(0),AL4(NOMESS-WORKD)             
         DC    C'SYSTEM    ',AL1(5),AL1(0),AL1(CARDRTNQ),AL4(SYSVAL)            
         DC    C'TSTRUN    ',AL1(5),AL1(0),AL1(0),AL4(TSTRUN-WORKD)             
         DC    C'USEWTO    ',AL1(5),AL1(0),AL1(0),AL4(USEWTO-WORKD)             
         DC    AL1(EOTQ)                                                        
                                                                                
         DS    0H                                                               
T01FIL   DCB   DDNAME=T01FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T01SNAM  EQU   T01FIL+40          DDNAME is at DCB+40                           
T01SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T01NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T02FIL   DCB   DDNAME=T02FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T02SNAM  EQU   T02FIL+40          DDNAME is at DCB+40                           
T02SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T02NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T03FIL   DCB   DDNAME=T03FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T03SNAM  EQU   T03FIL+40          DDNAME is at DCB+40                           
T03SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T03NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T04FIL   DCB   DDNAME=T04FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T04SNAM  EQU   T04FIL+40          DDNAME is at DCB+40                           
T04SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T04NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T05FIL   DCB   DDNAME=T05FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T05SNAM  EQU   T05FIL+40          DDNAME is at DCB+40                           
T05SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T05NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T06FIL   DCB   DDNAME=T06FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T06SNAM  EQU   T06FIL+40          DDNAME is at DCB+40                           
T06SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T06NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T07FIL   DCB   DDNAME=T07FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T07SNAM  EQU   T07FIL+40          DDNAME is at DCB+40                           
T07SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T07NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T08FIL   DCB   DDNAME=T08FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T08SNAM  EQU   T08FIL+40          DDNAME is at DCB+40                           
T08SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T08NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T09FIL   DCB   DDNAME=T09FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T09SNAM  EQU   T09FIL+40          DDNAME is at DCB+40                           
T09SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T09NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T10FIL   DCB   DDNAME=T10FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T10SNAM  EQU   T10FIL+40          DDNAME is at DCB+40                           
T10SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T10NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T11FIL   DCB   DDNAME=T11FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T11SNAM  EQU   T11FIL+40          DDNAME is at DCB+40                           
T11SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T11NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T12FIL   DCB   DDNAME=T12FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T12SNAM  EQU   T12FIL+40          DDNAME is at DCB+40                           
T12SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T12NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T13FIL   DCB   DDNAME=T13FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T13SNAM  EQU   T13FIL+40          DDNAME is at DCB+40                           
T13SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T13NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T14FIL   DCB   DDNAME=T14FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T14SNAM  EQU   T14FIL+40          DDNAME is at DCB+40                           
T14SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T14NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T15FIL   DCB   DDNAME=T15FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T15SNAM  EQU   T15FIL+40          DDNAME is at DCB+40                           
T15SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T15NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T16FIL   DCB   DDNAME=T16FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T16SNAM  EQU   T16FIL+40          DDNAME is at DCB+40                           
T16SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T16NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T17FIL   DCB   DDNAME=T17FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T17SNAM  EQU   T17FIL+40          DDNAME is at DCB+40                           
T17SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T17NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T18FIL   DCB   DDNAME=T18FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T18SNAM  EQU   T18FIL+40          DDNAME is at DCB+40                           
T18SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T18NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T19FIL   DCB   DDNAME=T19FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T19SNAM  EQU   T19FIL+40          DDNAME is at DCB+40                           
T19SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T19NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T20FIL   DCB   DDNAME=T20FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T20SNAM  EQU   T20FIL+40          DDNAME is at DCB+40                           
T20SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T20NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T21FIL   DCB   DDNAME=T21FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T21SNAM  EQU   T21FIL+40          DDNAME is at DCB+40                           
T21SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T21NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T22FIL   DCB   DDNAME=T22FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T22SNAM  EQU   T22FIL+40          DDNAME is at DCB+40                           
T22SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T22NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T23FIL   DCB   DDNAME=T23FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T23SNAM  EQU   T23FIL+40          DDNAME is at DCB+40                           
T23SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T23NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T24FIL   DCB   DDNAME=T24FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T24SNAM  EQU   T24FIL+40          DDNAME is at DCB+40                           
T24SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T24NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T25FIL   DCB   DDNAME=T25FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T25SNAM  EQU   T25FIL+40          DDNAME is at DCB+40                           
T25SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T25NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T26FIL   DCB   DDNAME=T26FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T26SNAM  EQU   T26FIL+40          DDNAME is at DCB+40                           
T26SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T26NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T27FIL   DCB   DDNAME=T27FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T27SNAM  EQU   T27FIL+40          DDNAME is at DCB+40                           
T27SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T27NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T28FIL   DCB   DDNAME=T28FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T28SNAM  EQU   T28FIL+40          DDNAME is at DCB+40                           
T28SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T28NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T29FIL   DCB   DDNAME=T29FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T29SNAM  EQU   T29FIL+40          DDNAME is at DCB+40                           
T29SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T29NAME  DC    CL44' '                                                          
                                                                                
         DS    0H                                                               
T30FIL   DCB   DDNAME=T30FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T30SNAM  EQU   T30FIL+40          DDNAME is at DCB+40                           
T30SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T30NAME  DC    CL44' '                                                          
                                                                                
T31FIL   DCB   DDNAME=T31FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T31SNAM  EQU   T31FIL+40          DDNAME is at DCB+40                           
T31SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T31NAME  DC    CL44' '                                                          
                                                                                
T32FIL   DCB   DDNAME=T32FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T32SNAM  EQU   T32FIL+40          DDNAME is at DCB+40                           
T32SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T32NAME  DC    CL44' '                                                          
                                                                                
T33FIL   DCB   DDNAME=T33FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T33SNAM  EQU   T33FIL+40          DDNAME is at DCB+40                           
T33SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T33NAME  DC    CL44' '                                                          
                                                                                
T34FIL   DCB   DDNAME=T34FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T34SNAM  EQU   T34FIL+40          DDNAME is at DCB+40                           
T34SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T34NAME  DC    CL44' '                                                          
                                                                                
T35FIL   DCB   DDNAME=T35FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T35SNAM  EQU   T35FIL+40          DDNAME is at DCB+40                           
T35SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T35NAME  DC    CL44' '                                                          
                                                                                
T36FIL   DCB   DDNAME=T36FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T36SNAM  EQU   T36FIL+40          DDNAME is at DCB+40                           
T36SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T36NAME  DC    CL44' '                                                          
                                                                                
T37FIL   DCB   DDNAME=T37FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T37SNAM  EQU   T37FIL+40          DDNAME is at DCB+40                           
T37SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T37NAME  DC    CL44' '                                                          
                                                                                
T38FIL   DCB   DDNAME=T38FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T38SNAM  EQU   T38FIL+40          DDNAME is at DCB+40                           
T38SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T38NAME  DC    CL44' '                                                          
                                                                                
T39FIL   DCB   DDNAME=T39FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T39SNAM  EQU   T39FIL+40          DDNAME is at DCB+40                           
T39SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T39NAME  DC    CL44' '                                                          
                                                                                
T40FIL   DCB   DDNAME=T40FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T40SNAM  EQU   T40FIL+40          DDNAME is at DCB+40                           
T40SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T40NAME  DC    CL44' '                                                          
                                                                                
T41FIL   DCB   DDNAME=T41FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T41SNAM  EQU   T41FIL+40          DDNAME is at DCB+40                           
T41SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T41NAME  DC    CL44' '                                                          
                                                                                
T42FIL   DCB   DDNAME=T42FIL,DSORG=PS,MACRF=PM,RECFM=VB,LRECL=8192,    +        
               BLKSIZE=0                                                        
T42SNAM  EQU   T42FIL+40          DDNAME is at DCB+40                           
T42SPACE DS    XL6                TRK,(.,.) should be enough                    
         DC    AL3(1,1)                                                         
T42NAME  DC    CL44' '                                                          
                                                                                
         ENTRY SSB                                                              
                                                                                
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOSTAT2                                                         
         DC    AL1(0)                                                           
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
                                                                                
$$CODE   LOCTR ,                                                                
                                                                                
*********************************************************************           
* Equates                                                           *           
*********************************************************************           
                                                                                
EOTQ     EQU   0                                                                
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
SPACEQ   EQU   C' '                                                             
DELIMQ   EQU   C'.'                                                             
TIMEQ    EQU   C'T'                                                             
DATEQ    EQU   C'D'                                                             
RECTYPQ  EQU   C'A'                                                             
                                                                                
CARDTABD DSECT ,                                                                
CARDKEY  DS    CL10                                                             
CARDKXLN DS    XL1             Length of key                                    
CARDVXLN DS    XL1             Length of field to move directly                 
CARDIND  DS    XL1                                                              
CARDRTNQ EQU   X'80'           Call routine to validate field                   
CARDNUMQ EQU   X'40'           Number comes before the delimeter                
CARDVDSP DS    XL4             Routine to validate field                        
CARDTBLQ EQU   *-CARDTABD                                                       
                                                                                
WORKD    DSECT                 ** GLOBAL WORKING STORAGE **                     
                                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DUBX     DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PL16     DS    PL16                                                             
DMCB     DS    6F                                                               
WORK     DS    XL80                                                             
TEMP     DS    XL256                                                            
TRTAB    DS    XL256                                                            
VDATCON  DS    V                                                                
VDATVAL  DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VCARDS   DS    V                                                                
VLOGIO   DS    V                                                                
VPRINTER DS    V                                                                
VPRNTBL  DS    V                                                                
VUTL     DS    V                                                                
VNUMVAL  DS    V                                                                
VLOADER  DS    V                                                                
VDYNALLO DS    V                                                                
VMQRPT   DS    V                                                                
ASSB     DS    A                                                                
                                                                                
ALLOCAL3 DS    0XL6                                                             
ALLO1AL3 DS    AL3                                                              
ALLO2AL3 DS    AL3                                                              
RETCODE  DS    XL1                                                              
ANYCARDS DS    CL1                                                              
                                                                                
DSPACE   DS    CL1                                                              
ANARUN   DS    CL1                                                              
TSTRUN   DS    CL1                                                              
T_ANAQ   EQU   C'A'                                                             
T_INPQ   EQU   C'I'                                                             
T_YESQ   EQU   YESQ                                                             
NOMESS   DS    CL1                                                              
LARGE    DS    CL1                                                              
L_LOADQ  EQU   C'L'                                                             
L_YESQ   EQU   YESQ                                                             
DSN      DS    CL8                                                              
DSNLN    DS    XL1                                                              
SYSTEM   DS    CL5                                                              
SYSLEN   DS    XL1                                                              
CTRYCODE DS    CL2                                                              
USEWTO   DS    CL1                                                              
KEEPS    DS    0C                                                               
KEEP1    DS    CL5                                                              
KEEP2    DS    CL5                                                              
KEEP3    DS    CL5                                                              
KEEP4    DS    CL5                                                              
KEEP5    DS    CL5                                                              
KEEP6    DS    CL5                                                              
KEEP7    DS    CL5                                                              
KEEP8    DS    CL5                                                              
KEEP9    DS    CL5                                                              
KEEPSLQ  EQU   *-KEEPS                                                          
KEEP#Q   EQU   KEEPSLQ/L'KEEP1                                                  
                                                                                
COUNTALL DS    PL8                                                              
COUNTFIL DS    PL8                                                              
                                                                                
CUR_TI6  DS    0CL6                                                             
CUR_TIM  DS    CL8                                                              
CUR_DAT  DS    0CL8                                                             
         DS    CL2                                                              
CUR_DA6  DS    CL6                                                              
                                                                                
CUR_REC  DS    (8192+4)X                                                        
         ORG   CUR_REC                                                          
CUR_RLN  DS    XL2                                                              
         DS    XL2                                                              
CUR_RTY  DS    CL5                                                              
         DS    XL1                                                              
CUR_ACT  DS    CL1                                                              
CUR_A_DQ EQU   C'D'                (DXTRACT action 'Delete')                    
         ORG   CUR_REC+8192+4                                                   
                                                                                
         DS    0H                                                               
AREA     DS    XL2000                                                           
                                                                                
         DS    0H                                                               
DSNBUFF  DS    (DSNBLEN*DSNBMAX)X                                               
                                                                                
ANABUFF  DS    0H                                                               
         DS    (ANABLNQ*ANABMXQ)X                                               
                                                                                
WORKX    DS    0D                                                               
                                                                                
DSNBUFD  DSECT                                                                  
DSNBTYP  DS    CL5                                                              
DSNBDSN  DS    CL44                                                             
DSNBCNT  DS    PL6                                                              
DSNBSIZ  DS    PL10                                                             
DSNBLEN  EQU   *-DSNBUFD                                                        
DSNBMAX  EQU   ((RECTABX-RECTAB)/L'AGXCPYDQ)+1                                  
                                                                                
ANABUFD  DSECT                                                                  
ANABTYP  DS    CL5                                                              
ANABSIZ  DS    PL8                                                              
ANABLNQ  EQU   *-ANABUFD                                                        
ANABMXQ  EQU   DSNBMAX                                                          
                                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
                                                                                
                                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019AGXSPLIT  07/06/20'                                      
         END                                                                    
