*          DATA SET SPSIDIO    AT LEVEL 040 AS OF 05/01/02                      
*PHASE T00A34A,+0                                                               
         TITLE 'SIDIO - I/O CONTROL MODULE FOR SID/EBD RECORDS'                 
SIDIO    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 100,**SIIO**,RR=R2                                               
         ST    R2,RELO                                                          
         USING MODD,RC                                                          
         MVC   USERRD,4(RD)        SAVE USERS RD                                
         SPACE 1                                                                
         L     R9,0(R1)            USER PASSES A(SIDBLOCK)                      
         USING SIDIOD,R9                                                        
         SPACE 1                                                                
         CLI   SIDINIT,0                                                        
         BE    IN2                                                              
         LA    R4,KEY              RESTORE KEYSAVE AND KEY                      
         USING ESDREC,R4                                                        
         XC    KEY,KEY                                                          
         MVC   ESDKTYPE(2),=X'0D59'                                             
         MVC   ESDKAM(11),SIACTAM                                               
         MVC   KEYSAVE,KEY                                                      
         B     READ2                                                            
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 3                                                                
IN2      MVI   SIMODE,SINORECS                                                  
         MVI   SIDINIT,1                                                        
         SPACE 1                                                                
         L     R1,SIACOM                                                        
         USING COMFACSD,R1                                                      
         MVC   SIDTAMGR,CDATAMGR   DATAMGR FROM COMFACS                         
         L     RF,CCALLOV                                                       
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A0F'                                           
*                                  CALLOV FOR UNDAY                             
         GOTO1 (RF),DMCB                                                        
         MVC   SIUNDAY,DMCB                                                     
         XC    DMCB(4),DMCB                                                     
         MVI   DMCB+7,X'11'                                                     
         BASR  RE,RF               AND UNTIME                                   
         MVC   SIUNTIME,DMCB                                                    
         XC    DMCB(4),DMCB                                                     
         MVI   DMCB+7,QSTAPACK                                                  
         BASR  RE,RF               AND STAPACK                                  
         MVC   SISTAPAK,DMCB                                                    
         SPACE 1                                                                
         XC    KEY,KEY             GET AN EBD RECORD                            
         LA    R4,KEY                                                           
         USING EBDREC,R4                                                        
         MVC   EBDKTYPE(2),=X'0D51'                                             
         MVC   EBDKAM,SISELAM                                                   
         L     R6,SIAEBD           IF AN EBD BUFFER IS PROVIDED                 
         LTR   R6,R6                                                            
         BZ    INEND                                                            
         BAS   RE,HIGH                                                          
         CLC   KEY(3),KEYSAVE                                                   
         BNE   INEND                                                            
         BAS   RE,GETREC                                                        
         SPACE 1                                                                
INEND    MVC   SIACTAM(6),SISELAM                                               
         OC    SISELSTA,SISELSTA   IF STATION IS SELECTED                       
         BZ    READ                                                             
         OC    SISELMKT,SISELMKT   BUT NOT MARKET                               
         BNZ   READ                                                             
         BAS   RE,GETSTA           GO AND FIND MARKET NUMBER                    
         LA    R4,IO               FROM AGENCY STATION RECORD                   
         USING STAREC,R4                                                        
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   SIACTMKT,DUB                                                     
         EJECT                                                                  
*              HANDLE I/O FOR SID RECORDS                                       
         SPACE 3                                                                
READ     LA    R4,KEY              BUILD INITIAL KEY                            
         USING ESDREC,R4                                                        
         XC    KEY,KEY                                                          
         MVC   ESDKTYPE(2),=X'0D59'                                             
         MVC   ESDKAM(6),SIACTAM                                                
         BAS   RE,HIGH                                                          
         MVI   SILCB,2             SET UP L'CONTROL-BREAK -1                    
         OC    SIACTMKT,SIACTMKT   2 IF ALL MARKETS                             
         BZ    READ4                                                            
         MVI   SILCB,4             4 IF ALL STATIONS                            
         OC    SIACTSTA,SIACTSTA                                                
         BZ    READ4                                                            
         MVI   SILCB,7             ELSE 7                                       
         B     READ4                                                            
         SPACE 1                                                                
READ2    BAS   RE,SEQ                                                           
         SPACE 1                                                                
READ4    ZIC   R1,SILCB            CHECK C/B                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   READEND                                                          
         SPACE 1                                                                
         MVC   SIACTAM(11),ESDKAM                                               
         BAS   RE,FILTKEY          FILTER ON KEY                                
         BNE   READ2                                                            
         L     R6,SIASID                                                        
         BAS   RE,GETREC                                                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   READ2                                                            
         BAS   RE,EBDFLESH         FLESH OUT DETAILS FROM EBD RECORD,           
         BAS   RE,RECFLESH         SID RECORD,                                  
         BAS   RE,STAFLESH         STATION                                      
         BAS   RE,MKTFLESH         AND MARKET                                   
         MVI   SIMODE,SIONEREC                                                  
         B     READSAVE                                                         
         SPACE 1                                                                
READEND  MVI   SIDINIT,0                                                        
         CLI   SIMODE,SINORECS          GIVE USER APPROPRIATE MODE              
         BE    XIT                                                              
         MVI   SIMODE,SINOMORE                                                  
         B     XIT                                                              
         SPACE 1                                                                
READSAVE B     XIT                                                              
         EJECT                                                                  
*              FILTER ON KEY FIELDS                                             
         SPACE 3                                                                
FILTKEY  NTR1                                                                   
         LA    R4,KEY                                                           
         USING ESDREC,R4                                                        
         CLI   ESDKNUM,0           DON'T WANT HEADER RECORDS                    
         BE    NO                                                               
         CLI   SIFLTPER,0          BUYING PERIOD                                
         BE    FILT2                                                            
         CLC   SIFLTPER,ESDKNUM                                                 
         BNE   NO                                                               
         SPACE 1                                                                
FILT2    LA    R0,1                                                             
         LA    R1,SIFLTDPT                                                      
         TM    SIOPTNS,X'40'       TEST FILTER LISTS USED                       
         BZ    *+12                                                             
         LA    R0,8                                                             
         LA    R1,SIDPLIST                                                      
*                                                                               
         CLI   0(R1),0             TEST ANY ENTRIES                             
         BE    FILT4               NO                                           
*                                                                               
         CLC   0(1,R1),ESDKDPT     MATCH ENTRY                                  
         BE    FILT4                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     NO                                                               
         SPACE 1                                                                
FILT4    LA    R0,1                                                             
         LA    R1,SIFLTPRG                                                      
         TM    SIOPTNS,X'40'       TEST FILTER LISTS USED                       
         BZ    *+12                                                             
         LA    R0,8                                                             
         LA    R1,SIPTLIST                                                      
*                                                                               
         CLI   0(R1),0             TEST ANY ENTRIES                             
         BE    YES                 NO                                           
*                                                                               
         CLC   0(1,R1),ESDKPRG     MATCH ENTRY                                  
         BE    YES                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     NO                                                               
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              FILTER ON RECORD                                                 
         SPACE 3                                                                
FILTREC  NTR1                                                                   
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING EDTELEM,R6                                                       
         CLI   SIFLTDAY,0                                                       
         BE    REC2                                                             
         CLC   SIFLTDAY,EDTDAY                                                  
         BNE   NO                                                               
         SPACE 1                                                                
REC2     OC    SIFLTTIM,SIFLTTIM                                                
         BZ    YES                                                              
         CLC   EDTTIME(2),SIFLTTIM                                              
         BL    NO                                                               
         MVC   DUB,EDTTIME+2                                                    
         OC    DUB(2),DUB                                                       
         BNZ   *+10                                                             
         MVC   DUB,EDTTIME                                                      
         CLC   DUB(2),SIFLTTIM+2                                                
         BH    NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              FLESH OUT DETAILS FROM EBD RECORD                                
         SPACE 3                                                                
EBDFLESH NTR1                                                                   
         MVI   ELCODE,X'02'        DAY PART NAME                                
         MVC   WORK(1),SIACTDPT                                                 
         BAS   RE,EBDEL                                                         
         MVC   SIDDPTNM,WORK+1                                                  
         SPACE 1                                                                
         MVI   ELCODE,X'03'        PROGRAM TYPE NAME                            
         MVC   WORK(1),SIACTPRG                                                 
         BAS   RE,EBDEL                                                         
         MVC   SIDPRGNM,WORK+1                                                  
         SPACE 1                                                                
         XC    SIUPDETS,SIUPDETS                                                
         MVI   ELCODE,X'05'        BUYING PERIOD                                
         MVC   WORK(1),SIACTPER                                                 
         BAS   RE,EBDEL                                                         
         MVC   SIDSTART(6),WORK+1                                               
         OC    SIACTCS1,SIACTCS1   START IS EFFECTIVE DATE                      
         BZ    XIT                                                              
         MVC   SIACTEF1,SIDSTART   FOR COST 1                                   
         B     XIT                                                              
         EJECT                                                                  
*              PICK OFF EBD ELEMENT VALUES                                      
         SPACE 3                                                                
EBDEL    NTR1                                                                   
         XC    WORK+1(8),WORK+1                                                 
         L     R6,SIAEBD                                                        
         LTR   R6,R6                                                            
         BZ    XIT                                                              
         BAS   RE,GETEL                                                         
         B     EBDEL4                                                           
         SPACE 1                                                                
EBDEL2   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
EBDEL4   BNE   XIT                                                              
         CLI   ELCODE,X'05'                                                     
         BNE   EBDEL6                                                           
         USING EBPELEM,R6                                                       
         CLC   EBPNUM,WORK                                                      
         BNE   EBDEL2                                                           
         SPACE 1                                                                
EBDEL5   MVC   WORK(8),EBPNUM                                                   
         CLI   EBPUPFIL,0                                                       
         BE    XIT                 ANY UPGRADE FOR PERIOD                       
         MVC   SIUPFILE,EBPUPFIL                                                
         MVC   SIUPEXP,EBPUPGRD                                                 
         MVC   SIUPBOOK,EBPUPFBK                                                
         MVC   SIUPDATA,EBPUPINP                                                
         MVI   SIUPWHER,C'E'                                                    
         B     XIT                                                              
         SPACE 1                                                                
EBDEL6   ZIC   R0,1(R6)            FIGURE OUT N'ENTRIES                         
         SH    R0,=H'2'                                                         
         SRL   R0,3                                                             
         SPACE 1                                                                
EBDEL8   CLC   2(1,R6),WORK                                                     
         BE    EBDEL5                                                           
         LA    R6,8(R6)                                                         
         BCT   R0,EBDEL8                                                        
         B     XIT                                                              
         EJECT                                                                  
*              FLESH OUT DETAILS FROM SID RECORD                                
         SPACE 3                                                                
RECFLESH NTR1                                                                   
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         USING EDTELEM,R6                                                       
         MVC   SIACTDAY,EDTDAY                                                  
         MVC   SIACTTIM,EDTTIME                                                 
         XC    SIACTCST,SIACTCST   COST FIELDS                                  
         MVC   SIACTCS1(4),EDTCOST ALWAYS MOVE FIRST COST                       
         CLI   EDTLEN,11                                                        
         BNH   *+10                                                             
         MVC   SIACTCS1(25),EDTCOST    MULTIPLE IF AVAILABLE                    
         SPACE 1                                                                
         L     R6,SIASID                                                        
         MVI   ELCODE,X'03'        IS THERE A SID UPGRADE OVERRIDE              
         BAS   RE,GETEL                                                         
         BNE   FLESH2                                                           
         USING SOUELEM,R6                                                       
         CLI   SOUUPFIL,X'41'      IS IT REAL                                   
         BL    FLESH2                                                           
         XC    SIUPDETS,SIUPDETS                                                
         MVC   SIUPFILE,SOUUPFIL                                                
         MVC   SIUPEXP,SOUUPGRD                                                 
         MVC   SIUPSTA,SOUUPSTA                                                 
         MVC   SIUPDAY,SOUUPDAY                                                 
         MVC   SIUPTIM,SOUUPTIM                                                 
         MVC   SIUPBOOK,SOUUPFBK                                                
         MVC   SIUPDATA,SOUUPINP                                                
         MVI   SIUPWHER,C'S'                                                    
         SPACE 1                                                                
FLESH2   MVC   SIDERDAY,SPACES                                                  
         MVC   SIDERTIM,SPACES                                                  
         GOTO1 SIUNDAY,DMCB,SIACTDAY,SIDERDAY                                   
         GOTO1 SIUNTIME,DMCB,SIACTTIM,SIDERTIM                                  
         L     R6,SIASID                                                        
*                                                                               
         MVC   SIACTCOM,SPACES                                                  
         TM    SIOPTNS,X'80'       TEST USER WANTS EXTENDED COMMENTS            
         BZ    *+10                                                             
         MVC   SIEXTCOM,SPACES     CLEAR THIS TOO STUPID                        
         MVI   ELCODE,X'04'        IS THERE A SID COMMENT/PROGRAM               
         BAS   RE,GETEL                                                         
         BNE   FLESH4                                                           
         USING COMELEM,R6                                                       
         ZIC   R1,COMLEN                                                        
         SH    R1,=H'3'                                                         
         CH    R1,=H'15'           ONLY INTERESTED IN 16 CHARACTERS             
         BL    *+8                                                              
         LA    R1,15                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SIACTCOM(0),COMTXT                                               
*                                                                               
         TM    SIOPTNS,X'80'       TEST USER WANTS EXTENDED COMMENTS            
         BZ    FLESH4                                                           
*                                                                               
         ZIC   R1,COMLEN                                                        
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SIEXTCOM(0),COMTXT                                               
         SPACE 1                                                                
FLESH4   B     XIT                                                              
         EJECT                                                                  
*              FLESH OUT STATION AND MARKET DETAILS                             
         SPACE 3                                                                
STAFLESH NTR1                                                                   
         CLC   SIACTMKT(5),SILSTMKT                                             
         BE    XIT                                                              
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         XC    0(32,R1),0(R1)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,SISELAGY                                                 
         MVC   STAPCTRY,SISELCTY                                                
         MVC   STAPMED,SISELMED                                                 
         MVC   STAPACOM,SIACOM                                                  
         MVC   STAPMKST,SIACTMKT                                                
*                                                                               
         GOTO1 SISTAPAK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SIACTMKT,STAPQMKT                                                
         MVC   SIDERSTA,STAPQSTA                                                
         DROP  R1                                                               
         BAS   RE,GETSTA                                                        
         LA    R4,IO                                                            
         USING STAREC,R4                                                        
         MVC   SIDERAFF,SNETWRK    PICK UP NETWORK AFFILIATION                  
         MVC   SILSTSTA,SIACTSTA                                                
         B     XIT                                                              
         SPACE 1                                                                
MKTFLESH NTR1                                                                   
         CLC   SIACTMKT(2),SILSTMKT                                             
         BE    XIT                                                              
         MVC   SILSTMKT,SIACTMKT                                                
         LA    R4,IO                                                            
         USING MKTREC,R4                                                        
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,C'M'                                                    
         MVI   MKTKMED,C'T'                                                     
         CLI   SISELMED,0                                                       
         BE    *+10                                                             
         MVC   MKTKMED,SISELMED                                                 
         MVC   MKTKMKT,SIDERMNO                                                 
         MVC   MKTKAGY,SISELAGY                                                 
         GOTO1 SIDTAMGR,DMCB,(X'C0',=C'DMRDHI'),=C'STATION',IO,IO,0             
         MVC   SIDERRNK,MKTRANK                                                 
         MVC   SIDERMNM,MKTNAME                                                 
         B     XIT                                                              
         EJECT                                                                  
*              DATA MANAGER AIDS                                                
         SPACE 3                                                                
HIGH     NTR1                                                                   
         MVC   KEYSAVE,KEY         SPTDIR CALLS                                 
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     DIR                                                              
         SPACE 1                                                                
SEQ      NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         SPACE 1                                                                
DIR      GOTO1 SIDTAMGR,DMCB,(X'C0',COMMAND),=C'SPTDIR',KEY,KEY,0               
         B     DMCHECK                                                          
         SPACE 1                                                                
GETREC   NTR1                                                                   
         GOTO1 SIDTAMGR,DMCB,(X'C0',=C'GETREC'),=C'SPTFILE',           X        
               KEY+14,(R6),DMWORK                                               
         B     DMCHECK                                                          
         SPACE 1                                                                
GETSTA   NTR1                                                                   
         LA    R4,IO                                                            
         USING STAREC,R4                                                        
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         CLI   SISELMED,0                                                       
         BE    *+10                                                             
         MVC   STAKMED,SISELMED                                                 
*                                                                               
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         XC    0(32,R1),0(R1)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,SISELAGY                                                 
         MVC   STAPCTRY,SISELCTY                                                
         MVC   STAPMED,SISELMED                                                 
         MVC   STAPACOM,SIACOM                                                  
         MVC   STAPMKST,SIACTMKT                                                
*                                                                               
         GOTO1 SISTAPAK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   STAKCALL,STAPQSTA                                                
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         DROP  R1                                                               
         MVC   STAKAGY,SISELAGY                                                 
         MVC   STAKCLT,=C'000'                                                  
         MVC   WORK(17),IO                                                      
         GOTO1 SIDTAMGR,DMCB,(X'C0',=C'DMRDHI'),=C'STATION',IO,IO,0             
         CLC   WORK(9),IO                                                       
         BE    DMCHECK                                                          
         MVI   SIDERROR,SINOSTA                                                 
         B     XIT                                                              
         SPACE 1                                                                
DMCHECK  CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),24,ELCODE                                                   
         SPACE 1                                                                
RELO     DS    F                                                                
SPACES   DC    CL132' '                                                         
         EJECT                                                                  
*              DSECT FOR SIDIO                                                  
         SPACE 3                                                                
MODD     DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
IO       DS    CL200                                                            
DMCB     DS    6F                                                               
DMWORK   DS    CL96                                                             
USERRD   DS    A                                                                
ELCODE   DS    CL1                                                              
COMMAND  DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE SPSIDIOD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENESD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENEBD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040SPSIDIO   05/01/02'                                      
         END                                                                    
