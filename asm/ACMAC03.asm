*          DATA SET ACMAC03    AT LEVEL 074 AS OF 11/18/93                      
*PHASE T61103A,*                                                                
*                                                                               
         TITLE 'T61103 - MULTIPLE A/C - ASSIGN FILTERS'                         
T61103   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,*MAC03**                                                     
         L     RA,0(R1)                    A(TWA)                               
         L     RC,4(R1)                    A(W/S)                               
         L     R8,8(R1)                    A(RECORD)                            
         USING MACWRKD,RC                                                       
         USING T611FFD,RA                                                       
         USING ACKEYD,R8                                                        
         CLI   MODE,EOF                                                         
         BE    BLDEND                                                           
         ZIC   R1,MODE                                                          
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     OKEXIT                      NO MODE 0                            
         B     BILDKEY                     NEW SCREEN                           
         B     MARKIT                      SCREEN AVAIL. FOR MARKING            
         B     BLDLINE                     RECORD FOR DISPLAY                   
OKEXIT   CR    RB,RB                                                            
         B     EXIT                                                             
ERREXIT  LTR   RB,RB                                                            
         B     EXIT                                                             
FASTEXIT L     RD,SAVED                                                         
EXIT     XMOD1                                                                  
         EJECT                                                                  
*              BUILD KEY FOR FIRST READ THIS PASS                               
BILDKEY  MVC   KEY,SPACES                                                       
         LA    R8,KEY                                                           
         MVI   IOMODE,SKSEQ1                                                    
         TWAXC MAFMARKH,MAFTBH,PROT=Y                                           
         SR    RE,RE                                                            
         LA    R7,MAFMARKH                                                      
         LA    RF,MAFTBH                                                        
         OI    1(R7),X'20'                                                      
         NI    6(R7),X'FE'                                                      
         IC    RE,0(R7)                                                         
         BXLE  R7,RE,*-12                                                       
         SR    R0,R0                                                            
         LR    R3,RA                                                            
         CLI   0(R3),0                                                          
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     *-14                                                             
         MVI   1(R3),1                     SPECIAL BITS FOR FAKPAK              
         MVI   2(R3),1                                                          
         CLI   PASS,0                                                           
         BNE   BILD100                                                          
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),WUNIT                                                   
         CLC   ACCFILT,SPACES              ANY START VALUE ?                    
         BE    *+14                        NO, USE FILTERS IF ANY               
         MVC   KEY+3(12),ACCFILT                                                
         B     BILD50                                                           
         LA    RF,KEY+3                    RF = START OF ACCOUNT                
         ZIC   RE,SLVLALEN                                                      
         BCTR  RE,0                                                             
         C     RE,=F'0'                                                         
         BL    BILD05                                                           
         LA    R1,CLIENT                   CLIENT HAS LEV A FILTER              
         EX    RE,KEYMOVE                                                       
*                                                                               
         LA    RF,1(RE,RF)                                                      
         IC    RE,SLVLBLEN                                                      
         BCTR  RE,0                                                             
         C     RE,=F'0'                                                         
         BL    BILD05                                                           
         LA    R1,PRODUCT                  PRODUCT "    " B    "                
         EX    RE,KEYMOVE                                                       
*                                                                               
         LA    RF,1(RE,RF)                                                      
         IC    RE,SLVLCLEN                                                      
         BCTR  RE,0                                                             
         C     RE,=F'0'                                                         
         BL    BILD05                                                           
         LA    R1,JOB                      JOB     "    " C    "                
         EX    RE,KEYMOVE                                                       
*                                                                               
         LA    RF,1(RE,RF)                                                      
         IC    RE,SLVLDLEN                                                      
         BCTR  RE,0                                                             
         C     RE,=F'0'                                                         
         BL    BILD05                                                           
         LA    R1,LEVEL4                   LEVEL4  "    " D    "                
         EX    RE,KEYMOVE                                                       
*                                                                               
BILD05   CLC   KEY+3(12),SPACES            IF NO FILTERS                        
         BE    BILD50                      THEN NO STOP POINT                   
         MVC   ENDKEY(15),KEY                                                   
         CLI   ENDKEY+14,C' '                                                   
         BNE   BILD50                                                           
         LA    RE,ENDKEY+13                                                     
         LA    RF,11                                                            
BILD10   CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         MVI   1(RE),X'FF'                                                      
         B     BILD50                                                           
         BCTR  RE,0                                                             
         BCT   RF,BILD10                                                        
         DC    H'0'                        NO KEY THERE AT ALL                  
BILD50   MVI   LASTLYN,0                                                        
         CLC   KEY+3(12),SPACES                                                 
         BNE   OKEXIT                                                           
         ZIC   R1,SKEYEND                  FORCE READ HIGH TO                   
         LA    R1,KEY(R1)                  SKIP OVER LEDGER RECORD              
         MVI   0(R1),X'41'                                                      
         B     OKEXIT                                                           
BILD100  MVC   ACKEYACC,LASTACCT           CONTINUATION SCREEN                  
         MVI   LASTLYN,0                                                        
         B     OKEXIT                                                           
KEYMOVE  MVC   0(0,RF),0(R1)               EXECUTED INSTRUCTION                 
         EJECT                                                                  
BLDLINE  ICM   R7,15,ADACBAL               ONLY NEED LOW-LEVEL ACCTS.           
         BZ    OKEXIT                                                           
         MVI   IOMODE,SKSEQ1               RE-SET READ MODE                     
         MVC   LASTACCT,ACKEYACC           SAVE FOR NEXT PASS                   
         MVC   MACHED1,FILHED1             MOVE HEADER FELDS                    
         MVC   MACHED2,FILHED2             TO SCREEN                            
         OI    MACHED1H+6,X'80'                                                 
         OI    MACHED2H+6,X'80'                                                 
         TM    ACSTATUS,X'80'                                                   
         BO    OKEXIT                      NO DELETED ACCOUNTS                  
         CLI   KEY,X'FE'                   I/O COUNT EXCEEDED                   
         BE    BLDEND                                                           
         USING FILLYND,R7                                                       
         ZIC   R7,LASTLYN                                                       
         LA    R6,FILTLNQ                                                       
         MR    R6,R6                                                            
         LA    R7,MAFMARKH(R7)                                                  
         NI    FILMARKH+1,X'DF'            UNPROTECT MARK FIELD                 
         OI    FILMARKH+6,X'80'            TRANSMIT                             
         MVI   FILTF1,X'40'                MOVE IN DOTS AND UNPROTECT           
         NI    FILTF1H+1,X'DF'             INPUT FIELDS                         
         MVI   FILTF2,X'40'                                                     
         NI    FILTF2H+1,X'DF'                                                  
         MVI   FILTF3,X'40'                                                     
         NI    FILTF3H+1,X'DF'                                                  
         MVI   FILTF4,X'40'                                                     
         NI    FILTF4H+1,X'DF'                                                  
         MVI   FILTF5,X'40'                                                     
         NI    FILTF5H+1,X'DF'                                                  
*        CLC   WUNIT(2),=C'SJ'                                                  
*        BNE   *+8                                                              
*        OI    FILTF3H+1,X'20'            PROTECT THE OFFICE                    
         OI    FILTF3H+6,X'80'                                                  
*                                                                               
         USING ACNAMED,R6                                                       
BLDNAM   ICM   R6,15,ADACNAM                                                    
         BZ    BLDSTAT                                                          
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     BLDSTAT                                                          
         MVC   FILNAME(0),2(R6)                                                 
*                                                                               
         USING ACSTATD,R6                                                       
BLDSTAT  ICM   R6,15,ADACSTAT                                                   
         BZ    BLDPROF                                                          
         CLI   ACSTFILT,X'40'                                                   
         BNH   *+10                                                             
         MVC   FILTF1,ACSTFILT             ACCOUNT FILTER 1                     
         CLI   ACSTFILT+1,X'40'                                                 
         BNH   *+10                                                             
         MVC   FILTF2,ACSTFILT+1           ACCOUNT FILTER2                      
*&&UK*&& CLC   WUNIT(2),=C'SJ'             PRODUCTION UNIT FILT3                
*&&UK*&& BE    BLDSTAT5                    COMES FROM PROFILE                   
         CLI   ACSTANAL,X'40'                                                   
         BNH   BLDSTAT5                                                         
         MVC   FILTF3,ACSTANAL             FILTER 3                             
BLDSTAT5 CLI   ACSTSUB,X'40'                                                    
         BNH   *+10                                                             
         MVC   FILTF4,ACSTSUB              FILTER 4                             
         CLI   ACSTLEN,ACSTLNQ2            FILTER 5 ONLY ON NEW ELEMENT         
         BL    BLDPROF                                                          
         CLI   ACSTFLT5,C' '                                                    
         BNH   *+10                                                             
         MVC   FILTF5,ACSTFLT5             FILTER 5                             
*                                                                               
         USING ACPROFD,R6                                                       
BLDPROF  ICM   R6,15,ADACPROF                                                   
         BZ    BLDCODE                                                          
         CLC   WUNIT(2),=C'SJ'                                                  
*&&UK*&& BNE   BLDCODE                                                          
*&&US*&& B     BLDCODE                                                          
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   FILTF3,ACPROFFC                                                  
*                                                                               
BLDCODE  MVC   FILACCT,ACKEYACC+3          ACCOUNT CODE                         
         ZIC   R7,LASTLYN                                                       
         LA    R7,1(R7)                                                         
         STC   R7,LASTLYN                  BUMP LINE #                          
         CLI   LASTLYN,FILMAX              BOTTOM OF THE SCREEN?                
         BNE   OKEXIT                                                           
BLDEND   ZIC   R7,PASS                     YES, BUMP PASS #                     
         LA    R7,1(R7)                                                         
         STC   R7,PASS                                                          
         LA    RF,MAFTBH-1                                                      
         LA    R7,MAFMARKH                                                      
BLDEND2  TM    1(R7),X'20'                 AND POSITION CURSOR TO               
         BNZ   BLDEND4                     FIRST UNPROT FIELD                   
         TM    6(R7),X'20'                                                      
         BO    BLDEND4                                                          
         CLI   8(R7),0                                                          
         BE    BLDEND6                                                          
BLDEND4  ZIC   RE,0(R7)                                                         
         BXLE  R7,RE,BLDEND2                                                    
BLDEND6  OI    6(R7),X'40'                                                      
         OI    MACACTH+6,X'81'             RE-TRANSMIT AND MODIFY               
         MVI   IOMODE,FINISHED             ACTION FIELD                         
         SR    R0,R0                                                            
         LR    R3,RA                                                            
         CLI   0(R3),0                     FIND BOTTOM OF THE TWA               
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     *-14                                                             
         MVI   1(R3),1                     AND PUT IN SPECIAL BITS              
         MVI   2(R3),1                                                          
         B     OKEXIT                                                           
         EJECT                                                                  
*              LOCK MARKED ACCOUNTS                                             
MARKIT   LA    R7,MAFMARKH                 A(FIRST SCREEN LINE)                 
         LA    R0,FILMAX                   MAX LINES                            
MARK005  CLI   FILMARK,0                                                        
         BE    MARK100                     NO MARK                              
         CLI   FILMARK,C'N'                                                     
         BE    MARK100                     DON'T MARK                           
         CLI   FILMARK,C'Y'                                                     
         BE    MARK010                     MARK                                 
         LA    RF,FILMARKH                 IF NONE OF THESE                     
         ST    RF,FADR                     THEN IT'S NOT ALLOWED                
         MVI   FERN,INVALID                                                     
         MVI   FNDX,0                                                           
         B     ERREXIT                                                          
*                                                                               
MARK010  DS    0H                                                               
MARK011  MVI   IOMODE,READ2                READ ACCOUNT INTO IO2                
         LA    R8,IO2                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY              BUILD KEY FOR READ                   
         MVC   KEY+1(2),WUNIT                                                   
         MVC   KEY+3(12),FILACCT                                                
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'FE'                   MAX COUNT EXCEEDED                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,ACRECORD                                                      
MARK015  CLI   0(R6),X'30'         REPLACE OLD X'30' ELEMS WITH                 
         BE    MARK018             ELEMENT OF NEW LENGTH                        
         CLI   0(R6),0                                                          
         BE    MARK020                                                          
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MARK015                                                          
*                                                                               
         USING ACSTATD,R6                                                       
MARK018  CLI   ACSTLEN,ACSTLNQ2    IF RECORD HAS OLD ELEMENT THEN               
         BNL   MARK020             DELETE OLD ELEMENT AND READD ELEM            
         XC    ELEMENT,ELEMENT     WITH THE NEW LENGTH                          
         ZIC   R1,ACSTLEN                                                       
         SH    R1,=H'1'                                                         
         EXMVC R1,ELEMENT,ACSTATD                                               
         LA    R6,ELEMENT                                                       
         MVI   ACSTLEN,ACSTLNQ2                                                 
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT'),('ACSTELQ',(R8)),0                 
         GOTO1 (RF),(R1),(C'P',=C'ACCOUNT'),(R8),ACSTATD,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,ACRECORD                                                      
MARK020  CLI   0(R6),X'30'              STATUS ELEMENT                          
         BE    MARK030                                                          
         CLI   0(R6),0                  E-O-R                                   
         BE    MARK050                                                          
*                                                                               
MARK021  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MARK020                                                          
*                                                                               
         USING ACSTATD,R6                                                       
MARK030  LA    RF,FILTF1H                                                       
         MVC   ACSTFILT(1),FILTF1          ACCOUNT FILTER 1                     
         CLI   ACSTFILT,X'4B'                                                   
         BE    MARKERR                                                          
         LA    RF,FILTF2H                                                       
         MVC   ACSTFILT+1(1),FILTF2        ACCOUNT FILTER 2                     
         CLI   ACSTFILT+1,X'4B'                                                 
         BE    MARKERR                                                          
*&&UK*&& CLC   WUNIT(2),=C'SJ'                                                  
*&&UK*&& BE    MARK030A                                                         
         LA    RF,FILTF3H                                                       
         MVC   ACSTANAL,FILTF3             ANALYSIS                             
         CLI   ACSTANAL,X'4B'                                                   
         BE    MARKERR                                                          
MARK030A LA    RF,FILTF4H                                                       
         MVC   ACSTSUB,FILTF4              FILTER 4                             
         CLI   ACSTSUB,X'4B'                                                    
         BE    MARKERR                                                          
         TM    FILTF5H+4,X'80'                                                  
         BNO   MARK021                                                          
         MVC   ACSTFLT5,FILTF5             FILTER 5                             
         B     MARK021                                                          
*                                                                               
MARK050  MVI   IOMODE,WRITE2               AND WRITE THE ACCOUNT BACK           
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'FE'                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
MARK100  LA    R7,FILTLNQ(R7)              NEXT LINE                            
         BCT   R0,MARK005                                                       
         B     OKEXIT                                                           
         SPACE 1                                                                
MARKERR  ST    RF,FADR                                                          
         MVI   FERN,INVALID                                                     
         MVI   FNDX,0                                                           
         B     ERREXIT                                                          
         EJECT                                                                  
FILHED1  DC    CL16'M    ACCOUNT',16C'-',C'NAME',16C'-'                         
         DC    C'    F  F  F  F  F         '                                    
FILHED2  DC    C'K  ',12C'-',40C' ',CL23' 1  2  3  4  5'                        
FILLYND  DSECT                                                                  
FILMARKH DS    CL8                                                              
FILMARK  DS    C                           MARK                                 
FILACCTH DS    CL8                                                              
FILACCT  DS    CL12                        ACCOUNT KEY EX. CUL                  
         DS    C                                                                
FILNAME  DS    CL36                        ACCOUNT NAME                         
         DS    C                                                                
FILTF1H  DS    CL8                                                              
FILTF1   DS    C                           ACCOUNT FILTER 1                     
FILTF2H  DS    CL8                                                              
FILTF2   DS    C                           ACCOUNT FILTER 2                     
FILTF3H  DS    CL8                                                              
FILTF3   DS    C                           ACCOUNT FILTER 3                     
FILTF4H  DS    CL8                                                              
FILTF4   DS    C                           ACCOUNT FILTER 4                     
FILTF5H  DS    CL8                                                              
FILTF5   DS    C                           ACCOUNT FILTER 5                     
FILTLNQ  EQU   *-FILLYND                                                        
       ++INCLUDE ACMACWRK                                                       
       ++INCLUDE ACMACEQU                                                       
         PRINT  OFF                                                             
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE FATWA                                                          
       ++INCLUDE FAFACTS                                                        
         PRINT  ON                                                              
FILMAX   EQU   17                          MAX. ON SCREEN                       
CLOSMAX  EQU   17                          MAX. ON SCREEN                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074ACMAC03   11/18/93'                                      
         END                                                                    
