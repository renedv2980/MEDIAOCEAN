*          DATA SET PPREP4502  AT LEVEL 038 AS OF 07/05/00                      
*PHASE PP4502C,+0,NOAUTO           ***** NOTE "C" PHASE                         
         TITLE 'PUB LISTS REPORT -- PP4500'                                     
*                                                                               
* KWAN   04/20/00 USE TEMP IO/STORAGE AREA FOR DUMMY GETRECS                    
*                                                                               
*    QOPT6    D=DELETE ELEMENT FOR "DELETED" PUBS (DDS ONLY)                    
*    QOPT7    Y=DUMP BEFORE AND AFTER (DON'T MARK FILE)  (DDS ONLY)             
*                                                                               
PP4502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP4502,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)            SET UP BASE REGISTERS                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         A     R9,=F'4096'                                                      
         USING PPFILED,RC,R9                                                    
         L     R7,ALISREC                                                       
         USING PLISREC,R7                                                       
         EJECT                                                                  
         CLI   MODE,LBUYREQ        LAST RECORD PROCESSED                        
         BNE   START50                                                          
         CLI   RENUMSW,1           RENUMBER PUB COUNT IN LISTREC ?              
         BNE   EXIT                NO                                           
         BAS   RE,RENUMRTN         YES - DO FINAL "RENUMBERING"                 
         B     EXIT                                                             
*                                                                               
*                                  TEST FOR ONE LIST ONLY                       
*                                                                               
START50  CLI   MODE,PROCLST        CHECK FOR LIST RECORD                        
         BNE   EXIT                                                             
*                                  TEST FOR ONE LIST ONLY                       
         CLC   QPAY(3),=C'ALL'                                                  
         BE    SAVE                                                             
         CLC   QPAY(3),SPACES                                                   
         BE    SAVE                                                             
         CLC   PLISKCOD,QPAY                                                    
         BNE   EXIT                                                             
SAVE     MVC   SAVEKEY,KEY         SAVE KEY TO RESTORE PPG                      
         MVI   DELSW,0             CLEAR DELETE SWITCH                          
         MVI   WRTSW,0             CLEAR WRITE  SWITCH                          
*                                                                               
         SPACE 2                                                                
CHECK    CLI   PLISKLIN,1          CHECK IF FIRST RECORD FOR LIST               
         BE    NEWLIST                                                          
         LA    R2,PLISREC+33                                                    
         B     CONTLIST                                                         
         EJECT                                                                  
NEWLIST  DS    0H                                                               
         CLI   RENUMSW,1           RENUMBER PUB COUNT IN LISTREC ?              
         BNE   *+8                 NO                                           
         BAS   RE,RENUMRTN         YES                                          
*                                                                               
         MVI   FORCEHED,C'Y'       PRINT HEADLINES                              
         MVC   PCNT,=H'1'                                                       
         SPACE 2                                                                
         LA    R2,PLISREC+33       FIND LIST DATE ELEMENT                       
         USING PLISDTEL,R2                                                      
         MVC   SAVEKEY,KEY         SAVE FOR PPG RESTORE                         
         SPACE 1                                                                
         SR    R0,R0                                                            
TSTDTELM CLI   0(R2),X'10'                                                      
         BE    GETDESC                                                          
         CLI   0(R2),X'00'                                                      
         BE    BADLIST                                                          
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     TSTDTELM                                                         
GETDESC  MVC   LISTNAME,PLISDESC   SAVE LIST DESCRIPTION                        
         SPACE 2                                                                
CONTLIST DS    0H                                                               
         CLI   0(R2),X'20'                                                      
         BE    GETDATA                                                          
         CLI   0(R2),X'00'                                                      
         BE    EXT                                                              
         B     NEXTELM                                                          
         EJECT                                                                  
GETDATA  DS    0H                  PUT OUT PUB CODE                             
         USING PLISPBEL,R2                                                      
GETPUBCD GOTO1 PUBEDIT,DMCB,PLISPUB,P+7,RR=RELO                                 
         XC    KEY,KEY             BUILD PUB KEY                                
         MVC   KEY(1),QMEDIA                                                    
         MVC   KEY+1(6),PLISPUB                                                 
         MVC   KEY+7(2),QAGENCY                                                 
READHIGH MVC   SAVEKEY2,KEY                                                     
         XC    KEY+9(23),KEY+9                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PUBDIR,KEY,KEY,(0,0)               
         SPACE 1                                                                
         CLC   KEY(7),SAVEKEY2     CHECK IF ANY RECORDS FOR PUB                 
         BNE   BADFILE                                                          
         SPACE 1                                                                
         CLI   KEY+9,X'81'         LOOK FOR PROPER PUB RECORD                   
         BNE   SETTOZZ                                                          
         SPACE 1                                                                
         CLC   KEY+7(2),QAGENCY    CHECK IF 81 RECORD HAS RIGHT AGY             
         BE    READFILE                                                         
         SPACE 1                                                                
         CLC   KEY+7(2),=C'ZZ'     CHECK IF 81 RECORD HAS SRDS AGENCY           
         BNE   SETTOZZ                                                          
         MVC   P+100(8),=C'**STND**'                                            
         B     READFILE                                                         
         SPACE 1                                                                
SETTOZZ  MVC   KEY+7(2),=C'ZZ'                                                  
         B     READHIGH                                                         
         EJECT                                                                  
READFILE GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PUBFILE,KEY+27,PUBREC,    X        
               (0,DMWORK)                                                       
         CLI   DMCB+8,X'80'                                                     
         BE    BADREC                                                           
FINDNMEL LA    R3,PUBREC+33        FIND PUB NAME ELEMENT                        
         USING PUBNAMEL,R3                                                      
         SPACE 1                                                                
         SR    R0,R0                                                            
CHKNMEL  CLI   0(R3),X'10'         CHECK FOR PUB NAME ELEMENT                   
         BE    GETNMDAT                                                         
         CLI   0(R3),X'00'         CHECK FOR END OF RECORD-ERROR                
         BE    BADPUB                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHKNMEL                                                          
         SPACE 1                                                                
GETNMDAT MVC   P+30(20),PUBNAME    PUT OUT PUB INFO                             
         CLI   PUBKMED,C'N'                                                     
         BNE   GETDAT2                                                          
         MVC   P+51(16),PUBCITY                                                 
         MVC   P+68(2),PUBSTATE                                                 
         OC    PUBZNAME,SPACES                                                  
         CLC   PUBZNAME,SPACES                                                  
         BE    *+10                                                             
         MVC   PSECOND+30(20),PUBZNAME                                          
         B     RPRT                                                             
GETDAT2  MVC   P+55(20),PUBZNAME                                                
RPRT     DS    0H                                                               
         MVC   HEAD7+30(3),PLISKCOD                                             
         MVC   HEAD7+49(20),LISTNAME                                            
*                                                                               
         EDIT  PCNT,(4,P)                                                       
         MVI   P+4,C'.'                                                         
         LH    RE,PCNT                                                          
         LA    RE,1(RE)                                                         
         STH   RE,PCNT                                                          
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPT6,C'D'          DELETE ELEMENT (PUB NOT ON FILE) ?           
         BE    DELRTN                                                           
*                                                                               
NEXTELM  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     CONTLIST                                                         
BADLIST  DC    H'0'                                                             
         SPACE 1                                                                
BADPUB   DC    H'0'                                                             
         SPACE 1                                                                
BADREC   DC    H'0'                                                             
         SPACE 1                                                                
BADFILE  MVC   P+30(21),=C'***PUB NOT ON FILE***'                               
         XC    P+100(8),P+100                                                   
         MVI   DELSW,1                                                          
         B     RPRT                                                             
         SPACE 3                                                                
EXT      DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         CLI   WRTSW,1             REWRITE RECORD ?                             
         BNE   EXT50               NO                                           
         MVI   RENUMSW,1           RENUMBER PUB COUNT IN LIST                   
         MVC   RENUMKEY,SAVEKEY    FOR USE IN RENUMBER ROUTINE                  
         CLI   QOPT7,C'Y'          TEST RUN (DUMP RECORD) ?                     
         BE    EXT40               YES - NO REWRITE                             
*                                                                               
* LAST GETREC WAS PROBABLY FOR PUBFILE - DO A GETREC FOR RECORD                 
* TO BE REWRITTEN INTO DIFFERENT I/O AREA (PLISREC HAS BEEN CHANGED)            
* THIS IS JUST A DUMMY GETREC USING TEMPIO                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,KEY+27,TEMPIO,    X        
               (0,DMWORK)                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  REWRITE UPDATED RECORD                       
         GOTO1 DATAMGR,DMCB,(DMOUTBTS,PUTREC),PRTFILE,KEY+27,ALISREC,  X        
               (0,DMWORK)                                                       
         CLI   DMCB+8,0                                                         
         BE    EXT50                                                            
         DC    H'0'                                                             
*                                                                               
EXT40    DS    0H                  "FINAL" RECORD DUMP                          
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    EXT50               YES                                          
         AP    DUMPCNT,=P'1'                                                    
         MVC   P+02(11),=C'** FINAL **'                                         
         GOTO1 REPORT                                                           
         BAS   RE,DMPREC                                                        
*                                  RESTORE PPG SEQUENCE                         
EXT50    GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PRTDIR,KEY,KEY,(0,0)               
EXIT     DS    0H                                                               
         XMOD1 1                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
DELRTN   DS    0H                  DELETE ELEMENT (DDS ONLY) PROCESS            
         CLI   DELSW,1             DELETE ELEMENT INDICATED ?                   
         BNE   NEXTELM             NO                                           
         MVI   DELSW,0                                                          
         CLI   QOPT7,C'Y'          DUMP RECORD ?                                
         BNE   DELRT20             NO                                           
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    DELRT20             YES                                          
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+02(12),=C'** BEFORE **'                                        
         GOTO1 REPORT                                                           
         BAS   RE,DMPREC                                                        
*                                                                               
DELRT20  DS    0H                                                               
         GOTO1 RECUP,DMCB,(1,(R7)),(R2),0     DELETE ELEMENT                    
         MVI   WRTSW,1             SET FOR REWRITE                              
         CLI   QOPT7,C'Y'          DUMP RECORD ?                                
         BNE   CONTLIST            NO - CONTINUE                                
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    CONTLIST            YES                                          
*                                                                               
         MVC   P+02(12),=C'** AFTER ***'                                        
         GOTO1 REPORT                                                           
         BAS   RE,DMPREC                                                        
         B     CONTLIST            CONTINUE                                     
*                                                                               
         EJECT                                                                  
*                                                                               
RENUMRTN NTR1                 COUNT AND UPDATE NUMBER OF PUBS IN LIST           
         MVI   RENUMSW,0           CLEAR RENUMBER SWITCH                        
         XC    KEY,KEY                                                          
         MVC   KEY(10),RENUMKEY    AGY/MED/RC/CLT/LST                           
         MVI   KEY+10,X'01'        "START" OF LIST REC KEYS                     
         MVC   KEYSAVE,KEY                                                      
         LA    R4,0                FOR COUNT OF PUB ELEMENTS                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PRTDIR,KEY,KEY,(0,0)               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   LISTDA,KEY+27       SAVE RECORD ADDRESS                          
*                                                                               
RENUM10  GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,KEY+27,ALISREC,   X        
               (0,DMWORK)                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,PLISREC+33                                                    
         MVI   ELCODE,X'20'                                                     
         CLC   ELCODE,0(R2)                                                     
         BE    RENUM30                                                          
RENUM20  BAS   RE,NEXTEL                                                        
         BNE   RENUM50             NEXT LIST RECORD                             
RENUM30  LA    R4,1(R4)            ADD TO PUB COUNT                             
         B     RENUM20                                                          
*                                                                               
RENUM50  GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),PRTDIR,KEY,KEY,(0,0)               
         CLC   KEY(10),KEYSAVE     SAME LIST ?                                  
         BE    RENUM10             YES - GET RECORD                             
*                                  NO - FINISH PUB COUNT UPDATE                 
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,LISTDA,ALISREC,   X        
               (0,DMWORK)          GET RECORD FOR 'LINE' NUMBER 01              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RENUMKEY+10,X'01'                                                
         CLC   RENUMKEY(11),PLISREC                                             
         BE    *+6                                                              
         DC    H'0'                MUST BE SAME                                 
*                                                                               
         LA    R2,PLISREC+33                                                    
         USING PLISDTEL,R2                                                      
         CLI   0(R2),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   P+02(10),PLISREC                                                 
         MVC   P+15(32),=C'**** PUB LIST COUNT UPDATED ****'                    
         MVC   P+50(04),=C'OLD:'                                                
         EDIT  PLISNPBS,(3,P+55),ALIGN=LEFT                                     
         MVC   P+60(04),=C'NEW:'                                                
         EDIT  (R4),(3,P+65),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 REPORT                                                           
         CLI   QOPT7,C'Y'          TEST RUN (DUMP RECORD) ?                     
         BE    RENUMXT             YES - NO REWRITE                             
         STCM  R4,3,PLISNPBS       UPDATE NUMBER OF PUBS IN LIST                
         GOTO1 DATAMGR,DMCB,(DMOUTBTS,PUTREC),PRTFILE,LISTDA,ALISREC,  X        
               (0,DMWORK)                                                       
RENUMXT  DS    0H                  RESTORE PPG'S KEY                            
         MVC   KEY,SAVEKEY                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PRTDIR,KEY,KEY,(0,0)               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  AND RECORD                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,KEY+27,ALISREC,   X        
               (0,DMWORK)                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R7,ALISREC                                                       
         MVC   HALF,25(R7)        RECORD LENGTH                                 
         LH    R4,HALF                                                          
         LA    R5,0(R7,R4)                                                      
DMPREC2  DS    0H                                                               
         LR    R6,R5                                                            
         SR    R6,R7                                                            
         BNP   DMPRECX                                                          
         CH    R6,=H'32'                                                        
         BNH   *+8                                                              
         LA    R6,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R7),WORK,(R6),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R7)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R6,1(R6)                                                         
         GOTO1 REPORT                                                           
         LA    R7,0(R7,R6)                                                      
         B     DMPREC2                                                          
*                                                                               
DMPRECX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
SAVEKEY  DS    CL32                                                             
SAVEKEY2 DS    CL32                                                             
RENUMKEY DS    CL32                                                             
LISTNAME DS    CL20                                                             
DELSW    DS    CL1                                                              
WRTSW    DS    CL1                                                              
RENUMSW  DC    XL1'00'                                                          
ELCODE   DS    CL1                                                              
LISTDA   DS    F                                                                
DUMPCNT  DC    PL3'0'                                                           
PCNT     DC    H'1'                                                             
*                                                                               
TEMPIO   DC    1000X'00'           GENERAL IO/STORAGE AREA                      
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
       ++INCLUDE PPREPWORK                                                      
         EJECT                                                                  
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038PPREP4502 07/05/00'                                      
         END                                                                    
