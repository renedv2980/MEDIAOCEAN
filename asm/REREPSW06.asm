*          DATA SET REREPSW06  AT LEVEL 019 AS OF 11/12/03                      
*          DATA SET REREPSW06  AT LEVEL 157 AS OF 09/24/03                      
*PHASE RESW02C,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREPSW06 (RESW02C) --- REP POINT PERSON DISPLAY'               
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPSW06 --- REP POINT PERSON DISPLAY                    *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
* 12NOV03  (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RESW02   CSECT                                                                  
         NMOD1 MYWORKX-MYWORKD,**RESW**,R9,R7,RR=R5                             
         USING MYWORKD,RC                                                       
         LR    RE,RC                                                            
         AH    RE,=Y(RCVREC-MYWORKD)                                            
         ST    RE,ARCVREC                                                       
         AH    RE,=Y(REC-RCVREC)                                                
         ST    RE,AREC                                                          
*                                                                               
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
         SPACE 1                                                                
*    READ IN RECORDS FROM REP FILE TAPE AND PROCESS THEM                        
SW200    EQU   *                                                                
         L     R0,AREC                                                          
         SH    R0,=H'4'            POINT TO REC-4                               
         GET   FILEIN,(R0)         (ALLOW FOR 4-BYTE HEADER)                    
         SPACE 1                                                                
*                                                                               
         L     RF,RECCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RECCTR                                                        
         L     RF,RECCTR2                                                       
         LA    RF,1(RF)                                                         
         ST    RF,RECCTR2                                                       
         CLC   RECCTR,=F'5000'     DISPLAY EVERY 5000 RECORDS                   
         BNE   SW220                                                            
         XC    RECCTR,RECCTR       CLEAR CYCLE COUNTER                          
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  RECCTR2,(8,P+15)                                                 
         MVC   P+26(17),=C'RECORDS FROM TAPE'                                   
         GOTO1 REPORT                                                           
*                                                                               
*        SET 2X'00' AT EOR (FOR GETEL)                                          
*                                                                               
         SPACE 1                                                                
SW220    EQU   *                                                                
         L     RE,AREC                                                          
         SH    RE,=H'4'            POINT TO REC-4                               
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)       PUT ZERO AT END OF RECORD                    
*                                                                               
*                                                                               
         L     R3,AREC                                                          
         CLI   0(R3),X'31'         POINT PERSON RECORD?                         
         BH    SWEOF               ALL DONE: FINISHED                           
         BNE   SW200               NO  - SKIP IT                                
         BAS   RE,POINTPER         YES - PROCESS IT                             
*                                                                               
         B     SW200               GET NEXT RECORD                              
         SPACE 2                                                                
SWEOF    DS    0H'0'                                                            
         CLOSE FILEIN                                                           
         B     SWXIT                                                            
         EJECT                                                                  
******************************************************************              
*              POINT PERSON RECORDS - PURGE ONLY                 *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
POINTPER NTR1                                                                   
*        MVC   P+1(27),RPTPKEY                                                  
*        GOTO1 REPORT                                                           
         MVC   KEY,RPTPKEY         FIND THE RECORD ON FILE                      
         GOTO1 HIGHDIR             READ THE DIRECTORY                           
         CLC   KEY(27),KEYSAVE     KEY FOUND ON FILE?                           
         BE    PPER0800            YES - NO DISPLAY                             
         MVC   P+1(14),=C'POINT PERSON ='                                       
         MVC   P+20(27),RPTPKREC                                                
         GOTO1 REPORT                                                           
         MVC   P+1(07),=C'NAME = '                                              
         MVC   P+10(20),RPTPNAME                                                
         MVC   P+35(12),=C'FOR EDI USE?'                                        
         TM    RPTPFLG,X'20'       BLOCK EDI USE?                               
         BNO   PPER0020            NO                                           
         MVI   P+47,C'N'           YES - BLOCK IT                               
PPER0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(12),=C'TELEPHONE = '                                         
         MVC   P+20(20),RPTPFONE                                                
         MVC   P+35(03),=C'FAX'                                                 
         MVC   P+52(09),=C'FAX PREF?'                                           
         LA    R5,RPTPELEM                                                      
PPER0040 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    PPER0080            YES                                          
         CLI   0(R5),X'21'         FAX ELEMENT?                                 
         BE    PPER0060            YES                                          
         ZIC   RF,1(R5)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R5,RF                                                            
         B     PPER0040            GO BACK FOR NEXT                             
PPER0060 EQU   *                                                                
         MVC   P+39(12),RPTPFXFX-RPTPFXEM(R5)                                   
*                                  INSERT FAX NUMBER                            
         TM    RPTPFXFG-RPTPFXEM(R5),X'40'                                      
*                                  EMAIL PREF?                                  
         BO    PPER0080            YES                                          
         MVI   P+63,C'Y'           NO  - SET 'FAX PREF'                         
PPER0080 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(11),=C'REP CODE = '                                          
         MVC   P+20(2),RPTPREP                                                  
         GOTO1 REPORT                                                           
         MVC   P+1(11),=C'OFFICE   = '                                          
         MVC   P+20(2),RPTPOFF                                                  
         GOTO1 REPORT                                                           
         MVC   P+1(11),=C'S/P      = '                                          
         MVC   P+20(3),RPTPSPER                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(12),=C'LEAVE DATE ='                                         
         OC    RPTPLDAT,RPTPLDAT   LEAVE DATE?                                  
         BZ    PPER0100            NO                                           
         GOTO1 DATCON,DMCB,(2,RPTPLDAT),(5,P+20)                                
PPER0100 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(05),=C'EMAIL'                                                
         LA    R5,RPTPELEM                                                      
PPER0140 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    PPER0180            YES                                          
         CLI   0(R5),X'22'         EMAIL ELEMENT?                               
         BE    PPER0160            YES                                          
         ZIC   RF,1(R5)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R5,RF                                                            
         B     PPER0140            GO BACK FOR NEXT                             
PPER0160 EQU   *                                                                
         ZIC   RF,1(R5)            GET LENGTH OF ENTRY                          
         SH    RF,=H'3'            SET FOR MOVE BY LENGTH                       
         EX    RF,PPER0170                                                      
         B     PPER0180                                                         
PPER0170 MVC   P+20(0),2(R5)                                                    
PPER0180 EQU   *                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
PPER0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETSWI   LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               AREC,(0,DMWORK)                                                  
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
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     SWXIT                                                            
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     SWXIT                                                            
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     SWXIT                                                            
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=SWEOF                                                      
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 3                                                                
         DS    0H                  HALFWORD ALIGNMENT NEEDED.                   
*                                                                               
MYWORKD  DSECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
MASTREP  DS    A                                                                
SUBREP   DS    A                                                                
RECCTR   DS    F                                                                
RECCTR2  DS    F                                                                
*                                                                               
ELCODE   DS    CL1                                                              
MDATE    DS    CL3   (BINARY YMD MONDAY OF THIS WEEK (OR MON OF ASAT))          
*                                                                               
AREC     DS    A                                                                
ARCVREC  DS    A                                                                
*                                                                               
         DS    F                   LENGTH OF RECOVERY RECORD                    
       ++INCLUDE DMRCVRHDR                                                      
RCVREC   DS    CL6144              AREA FOR RECOVERY RECORD                     
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL6144              AREA FOR RECORD                              
         SPACE 2                                                                
MYWORKX  EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
*  INCLUDE REGENPTP                POINTPERSON RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENPTP          POINTPERSON RECORD                           
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019REREPSW06 11/12/03'                                      
         END                                                                    
