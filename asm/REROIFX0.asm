*          DATA SET REROIFX0   AT LEVEL 216 AS OF 05/01/02                      
*          DATA SET REROIFX0   AT LEVEL 214 AS OF 10/30/89                      
*PHASE REROIU0C,*                                                               
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE HEXIN                                                                  
ROIX     TITLE 'REROIUP0X -- OINK KEY DELETER'                                  
ROIU0    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**ROIUP0,VREGSAVE                                              
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
*                                                                               
         USING ROIU0+4096,R9,RA                                                 
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         B     MAIN100                                                          
STATMSG  DC    CL20' '             WINDOW FOR DUMPS                             
         SPACE 2                                                                
DUMPADDR DS    0F'0'               LIST OF ADDRESSES TO DUMP                    
         DC    A(0),A(DUMPADDR)                                                 
         DC    A(WORKD),V(HEXIN)                                                
         DC    X'00017428',X'80020000'                                          
         DC    2F'0'                                                            
         SPACE 2                                                                
VREGSAVE DC    V(REGSAVE)                                                       
         SPACE 2                                                                
MAIN100  EQU   *                                                                
         ST    RB,DUMPADDR                                                      
         GOTO1 =V(STXITER),DMCB,DUMPADDR                                        
*                                                                               
*- ALLOW CONCURRENT FILE UPDATING                                               
         GOTO1 DATAMGR,P1,=C'UPDID',=C'REP',FLIST,IOAREA                        
         L     RE,P4                                                            
         MVC   0(2,RE),=C'XX'                                                   
*                                                                               
*- OPEN DATAMGR                                                                 
         LA    RF,RPWCREC                                                       
         ST    RF,AIOAREA                                                       
         BAS   RE,ROIOPENU                                                      
*                                                                               
*- JUST LEAVE SJR PWC KEYS                                                      
PURGE    EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'2F'                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HDROI                                                            
*                                                                               
         SR    R5,R5                                                            
*                                                                               
         B     WPWC920                                                          
WPWC910  GOTO1 SDROI                                                            
WPWC920  EQU   *                                                                
         CLI   KEY,X'2F'                                                        
         BNE   WPWCEXT                                                          
*                                                                               
**       CLC   =C'SJ',KEY+25                                                    
**       BE    WPWC910                                                          
*                                                                               
         GOTO1 GFROI               READ IN RECORD                               
*                                                                               
*UNPURGE TM    RPWCCNTL,X'80'                                                   
*UNPURGE BZ    WPWC910                                                          
*                                                                               
****     GOTO1 =V(HEXOUT),P1,RPWCREC,P,40,=C'STD'                               
****     GOTO1 VPRINTER                                                         
         OI    RPWCCNTL,X'80'      MARK RECORD                                  
*UNPURGE XC    RPWCCNTL,X'80'      UNMARK RECORD                                
         GOTO1 PFROI                                                            
*                                                                               
         OI    KEY+31,X'80'        DELETE                                       
*UNPURGE XC    KEY+31,X'80'        UNDELETE                                     
         GOTO1 WDROI                                                            
WPWC950  LA    R5,1(R5)            COUNT DELETES                                
         B     WPWC910                                                          
*                                                                               
WPWCEXT  EQU   *                                                                
EXITBASE XBASE RC=(R5)                                                          
       ++INCLUDE RGENROI                                                        
         LTORG                                                                  
         SPACE 2                                                                
FLIST    DS    0H                  DATAMGR FILE LIST                            
         DC    CL8'UROIFILE'                                                    
         DC    CL8' ROIDIR '                                                    
         DC    CL8'X       '                                                    
         SPACE 2                                                                
PWCTYPE  DC    CL2'10'             SORT REC TYPE: PWC                           
*                                                                               
ATHTYPE  DC    CL2'20'             ATHENA                                       
         SPACE 2                                                                
SRTFLD   DC    CL80'SORT FIELDS=(4,30,BI,A),WORK=1 '                            
*                                                                               
RECTYP   DC    CL80'RECORD TYPE=V,LENGTH=(1000) '                               
*                                                                               
         DS    0F                                                               
         TITLE 'WORK AREA'                                                      
         DS    0F                                                               
WORKD    EQU   *                                                                
         DC    C'DMCB'                                                          
DMCB     DS    0CL24               PARAMETER LIST                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
         DC    CL4'DUB*'                                                        
DUB      DS    D                                                                
DOUBLE   DS    D                                                                
         DC    C'WORK'                                                          
WORK     DS    CL64                                                             
         DC    C'FULL'                                                          
FULL     DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
THREE    DS    CL3                                                              
BYTE     DS    CL1                                                              
         SPACE 1                                                                
* FILE HANDLING AREAS *                                                         
         SPACE 1                                                                
         DC    C'KEYS'                                                          
ROIKEY   DS    0CL36                                                            
KEY      DS    CL36                KEY                                          
ROIKEYSV DS    0CL36                                                            
KEYSAVE  DS    CL36                KEY SAVED BEFORE READ HIGH                   
DMWORK   DS    CL96                                                             
         SPACE 2                                                                
DMREAD   DC    CL8'DMREAD'         COMMANDS                                     
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
         SPACE 2                                                                
LASTFILE DS    F                   BETWEEN I/O CONTROLS                         
LASTDA   DS    F                                                                
LASTIO   DS    F                                                                
LASTLEN  DS    H                                                                
LASTKYST DS    H                                                                
DATADISP DS    H                   DISPLACEMENT TO FIRST ELEMENT                
DMINBTS  DS    CL1                 SET TO X'80'                                 
         EJECT                                                                  
* ADDRESS LIST *                                                                
         DC    0F'0'                                                            
         DC    C'ADDR'                                                          
DATAMGR  DC    V(DATAMGR)                                                       
PRINT    DC    V(PRINT)                                                         
DATCON   DC    V(DATCON)                                                        
DWRKRB   DC    A(MY4KBUF)          WORKER BUFFER                                
VPRINTER DC    V(PRINTER)                                                       
AWRKRB   DC    A(MY4KBUF)                                                       
ASORTREC DS    A                   A(SORTED RECORD)                             
         SPACE                                                                  
NUMSORT  DC    PL4'0'                                                           
         SPACE 2                                                                
         DC    C'SWITCHES*'                                                     
CTLDATE  DS    CL1                 CONTROL CARD PROCESS FLDS                    
CTLWRITE DS    CL1                 ('Y' = PROCESSED)                            
CTLPWC   DS    CL1                                                              
CTLATHEN DS    CL1                                                              
*                                                                               
MARKFILE DS    CL1                 C'Y' = UPDATE FILE. 'N'=REPORT ONLY          
PWC      DS    CL1                 C'Y' = UPDATE PWC RECORDS                    
ATHENA   DS    CL1                 C'Y' = UPDATE ATHENA RECORDS                 
*                                                                               
FIRSTSW  DS    CL1                 C'Y' IN 1ST PASS.                            
*                                                                               
FULLWEEK DS    CL1                 C'Y' = DO ALL INDICES FOR WEEK.              
*                                  C'N' = SINGLE DAY INDEX ONLY                 
*                                                                               
DELPWC   DS    CL1                 DELETE PWC INSTEAD OF WRITING BACK           
*                                                                               
STOP     DS    CL1                 STOP RUN IF 'Y'                              
RETCODE  DS    CL1                 RETURN CODE VALUE                            
*                                                                               
RUNDATE  DS    0CL6                TODAY IF FULLWEEK=Y                          
RUNYY    DS    CL2                 OR DAY FROM DATE CARD.                       
RUNMM    DS    CL2                                                              
RUNDD    DS    CL2                                                              
*                                                                               
WEEKOF   DS    CL6                 MONDAY START DATE                            
TODAY    DS    CL6                 CURRENT SYSTEM DATE                          
CURINDX  DS    CL6                 DATE ASSOCIATED WITH CURRENT INDEX           
*                                                                               
START    DS    CL6                 YYMMDD START DATE                            
END      DS    CL6                 YYMMDD END DATE                              
*                                                                               
WORKDATE DS    CL6                 USED BY FINDINDX                             
*                                                                               
SAVEDATE DS    CL3                 BINARY TAPE DATE.                            
*                                                                               
NUMPWC   DC    PL4'0'              # PWC RECS RELEASED TO SORT                  
NUMATHEN DC    PL4'0'                                                           
*                                                                               
WEEKDISP DS    F                   WEEK ACTIVITY DISPLACEMENT                   
*                                                                               
COMMAND  DS    CL8                 COMMAND FOR RGENIO                           
AIOAREA  DS    A                   FOR RGENIO                                   
         SPACE                                                                  
SAVEKEY  DS    CL8                 PWC SAVED SORT KEY                           
SAVEID   DS    CL1                 PWC SAVED KEY ID                             
OLDREC   DS    CL1                 PWC OLD/NEW RECORD FLAGS                     
NEWREC   DS    CL1                                                              
SAVEADDR DS    A                   PWC SAVED A(ELEMENT)                         
*                                                                               
PWCNUM   DS    F                   # SORT RECS IN PWC REC                       
PWCADD   DC    F'0'                # PWC RECS ADDED                             
PWCCHA   DC    F'0'                # PWC RECS CHANGED                           
PWCDEL   DC    F'0'                # PWC KEYS DELETED                           
         SPACE 2                                                                
ATHADD   DC    F'0'                # ATHENA RECS ADDED                          
ATHCHA   DC    F'0'                # ATHENA RECS CHANGED                        
ATHDEL   DC    F'0'                # ATHENA KEYS DELETED                        
         SPACE 2                                                                
*                                                                               
*- WORKER FILE STUFF                                                            
         DC    C'INDEX*'                                                        
INDEX    DS    CL16                                                             
         SPACE                                                                  
NDXNDX   EQU   0                   16 BYTE INDEX                                
NDXDATE  EQU   16                  YYMMDD DATE FOR THIS INDEX                   
*                                                                               
NDXLEN   EQU   22                  ENTRY LEN                                    
NDXSIZE  EQU   (16*NDXLEN)+2       TABLE SIZE                                   
*                                                                               
INDEXTBL DC    (NDXSIZE)X'00'                                                   
LNDEXTBL EQU   *-INDEXTBL                                                       
         SPACE 2                                                                
         DC    C'IOAREA*'                                                       
*                                                                               
IOAREA   DS    2000X                                                            
         ORG   IOAREA                                                           
       ++INCLUDE REGENPWC          PWC RECORD DSECT                             
         ORG   IOAREA+2000                                                      
         DC    C'SORTREC*'                                                      
         SPACE                                                                  
         DS    0F                                                               
SORTREC  DS    3000X                                                            
         DC    C'4KBUF*'                                                        
MY4KBUF  DS    4096X               4K WORKER BUFFER                             
         SPACE                                                                  
         ENTRY UTL                 FOR DATAMGR                                  
UTL      DC    F'0',X'08'          REP FILE                                     
         SPACE                                                                  
         ENTRY SSB                 TURN OFF ROI FILE RECOVERY                   
SSB      DC    F'2'                                                             
*                                                                               
         SPACE 2                                                                
SORTRECD DSECT                                                                  
         DS    F                   REC LEN/CONTROL BYTE                         
SORTKEY  DS    0CL30                                                            
SRTKTYPE DS    CL2                 RECORD TYPE. '10'=PWC, '20'=ATHENA           
SRTKDATA DS    CL28                SORT KEY DATA                                
LSORTKEY EQU   *-SORTKEY                                                        
*                                                                               
*- PWC SORT KEY                                                                 
         ORG   SRTKDATA                                                         
SRTPREP  DS    CL2                 REP CODE                                     
SRTPCON  DS    CL4                 CONTRACT NUMBER                              
SRTPID   DS    CL1                 SORT REC ID                                  
*                                                                               
*- ATHENA SORT KEY  (TO BE DETERMINED)                                          
*                                                                               
         ORG   SRTKDATA                                                         
SRTADATA DS    CL28                                                             
*                                                                               
*- SORT RECORD DATA (VARIABLE WORKER RECORD)                                    
         ORG   SORTKEY+LSORTKEY                                                 
WRKREC   DS    0C                                                               
         DS    XL4                 REC LEN/BYTE OF 0                            
         SPACE 2                                                                
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    CL27                                                             
         DS    2050C                                                            
       ++INCLUDE REDPRINTD                                                      
         SPACE 2                                                                
       ++INCLUDE DMWRKRK                                                        
         SPACE 2                                                                
       ++INCLUDE DMWRKRD                                                        
*                                                                               
*- RELWEEK WORK DSECT                                                           
RELWEEKD DSECT                                                                  
RWDATE1  DS    CL6                 DATE 1, 6 BYTE EBCDIC                        
RWDATE2  DS    CL6                 DATE 2, 6 BYTE EBCDIC                        
RWWORK   DS    CL6                 WORK AREA                                    
*                                                                               
RWJDATE1 DS    CL4                 DATE 1 JULIAN                                
RWJDATE2 DS    CL4                 DATE 2 JULIAN                                
RWDAYS   DS    F                                                                
*                                                                               
RWDLEN   EQU   *-RELWEEKD          LENGTH OF RELWEEK DSECT                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'216REROIFX0  05/01/02'                                      
         END                                                                    
