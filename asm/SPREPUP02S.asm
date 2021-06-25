*          DATA SET SPREPUP02S AT LEVEL 089 AS OF 05/01/02                      
*PHASE SPUP02S                                                                  
SPUP02S  TITLE 'SPREPUP02S - MATCH AGENCY/NETWORK AGAINST AMS FILE'             
SPUP02   CSECT                                                                  
         DS    8192C                                                            
         ORG   *-8192                                                           
         PRINT NOGEN                                                            
*                                  GOT 3 BASE REGISTERS                         
         NMOD1 0,SPUP02,R8,R7                                                   
*                                                                               
         LR    RC,RB               SET UP MY WORK AREA                          
         AH    RC,=Y(SPUPWRKD-SPUP02)                                           
         USING SPUPWRKD,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*     MAIN SECTION OF THE PROGRAM                                               
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         LR    R6,RB               SET ADDRESS OF RECORD FOR SPONSOR            
         A     R6,=A(SPOTREC-SPUP02)                                            
         ST    R6,AREC1                                                         
         LR    R6,RB               SET ADDRESS OF RECORD #2 FOR SPONSOR         
         A     R6,=A(SPOTREC2-SPUP02)                                           
         ST    R6,AREC2                                                         
         ST    R6,ADSTAT                                                        
         ST    R6,ADSTATAD                                                      
*                                                                               
         MVC   AREC,AREC1          DEFAULT RECORD AREA                          
*                                                                               
         ZAP   COUNTER1,=P'0'      # OF RECORDS THAT HAVE PROBLEMS              
         XC    SVDHDEND,SVDHDEND                                                
*                                                                               
         MVC   P+2(8),=C'AGENCY: '                                              
         MVC   P+12(2),SVAGY                                                    
         GOTO1 REPORT                                                           
         MVC   P+2(12),=12C'-'                                                  
         GOTO1 REPORT                                                           
*                                                                               
         OPEN  (FILEIN,INPUT)                                                   
         XCEFL LINDATA,4000                                                     
         XCEFL NEWTAB,1000                                                      
         XCEFL OLDTAB,1000                                                      
*                                                                               
         MVC   SVCALL,=C'0007'     STARTING POINT OF DATASET                    
*                                  CHEATING A LITTLE, SAVES CODE.               
         LA    R3,NEWTAB                                                        
         LA    R2,LINDATA                                                       
         USING INPUTD,R2                                                        
MAIN01   GET   FILEIN,NETLINE      ONE LINE AT A TIME                           
         CLC   SVCALL,INCALL       SAME CALL LETTER STILL?                      
         BE    MAIN02              YES? GET ALL NEW NETWORKS                    
         MVI   0(R3),X'FF'         NO?  GET ALL OLD NETWORKS                    
*                                                                               
         BAS   RE,READRECS         SAVE STATION NETWORKS TO OLDTAB              
*                                                                               
         MVC   SVCALL,INCALL       START OVER WITH NEW CALL LETTER              
         XCEFL NEWTAB,1000                                                      
*                                                                               
         LA    R3,NEWTAB                                                        
MAIN02   MVC   SVNET,INNTWK        DATASET'S IN WEIRD FORMAT OF                 
         CLI   SVNET+2,C'"'        ####,"#',"NET"                               
         BNE   MAIN05                                                           
         MVC   SVNET+2(2),=X'4040'                                              
MAIN05   CLI   SVNET+3,C'"'                                                     
         BNE   MAIN07                                                           
         MVI   SVNET+3,X'40'                                                    
MAIN07   BAS   RE,NETCONV                                                       
         MVC   0(4,R3),SVNET                                                    
         LA    R3,4(R3)                                                         
         B     MAIN01                                                           
*                                                                               
NOMORE   GOTO1 REPORT              SPACE                                        
         GOTO1 REPORT              SPACE                                        
         GOTO1 REPORT              I NEED SPACE!                                
         MVC   P+2(41),=CL42'NUMBER OF STATIONS WITH MISSING NETWORKS:'         
         EDIT  (P8,COUNTER1),(17,P+45),ALIGN=LEFT,ZERO=NOBLANK                  
         GOTO1 REPORT                                                           
*                                                                               
NOMOREX  CLOSE FILEIN                                                           
         MVI   MODE,REQLAST                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS THROUGH ALL THE RECORDS ON THE SPOT FILE                   
***********************************************************************         
*                                                                               
READRECS NTR1                                                                   
*                                                                               
         LA    R1,OLDTAB                                                        
         ST    R1,TABPTR                                                        
*                                                                               
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVC   STAKEY(2),=C'ST'    ST                                           
         MVC   STAKCALL,SVCALL     ####                                         
         MVI   STAKCALL+4,C'T'     T                                            
         MVC   STAKAGY,SVAGY       AGENCY                                       
         MVC   SVKEY1,KEY                                                       
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGH                                                       
RREC10   L     R6,AREC                                                          
         USING STAREC,R6                                                        
         CLC   STAKEY(9),SVKEY1                                                 
         BE    RREC12                                                           
         BL    RREC300                                                          
         B     RRECX                                                            
*                                                                               
*****                                                                           
* SHOW CABLE STATION AND NAME                                                   
*****                                                                           
RREC12   CLI   QOPT3,C'Y'          DON'T SHOW DEACTIVATED STATIONS?             
         BNE   RREC15                                                           
         CLI   SSYSDACT,X'FF'      THIS STATION DEACTIVATED?                    
         BE    RREC200             YES, SKIP THIS RECORD                        
*                                                                               
RREC15   DS    0H                                                               
         OC    SSYSNAME,SSYSNAME                                                
         BZ    RREC30                                                           
         ZICM  R3,STAKLEN,2                                                     
         LA    RE,SCBLSQNQ                                                      
         CR    R3,RE               RECORD LENGTH IS X'0113' OR X'0160'?         
         BNE   RREC16                                                           
         OC    SCBL24,SCBL24                                                    
         BNZ   *+14                                                             
         OC    SCBLSEQ,SCBLSEQ                                                  
         BZ    RREC30                                                           
         XC    WKTOP24,WKTOP24                                                  
         XC    WKCBLSEQ,WKCBLSEQ                                                
         MVC   WKTOP24,SCBL24                                                   
         MVC   WKCBLSEQ,SCBLSEQ                                                 
         MVI   MYFLAG,C'Y'         FLAG SET TO YES FOR REC LEN X'0160'          
         B     RREC17                                                           
*                                                                               
RREC16   MVI   MYFLAG,C'N'         FLAG SET TO NO FOR REC LEN X'0113'           
*                                                                               
         OC    SSYSNETS(16),SSYSNETS                                            
         BZ    RREC30                                                           
RREC17   CLI   QOPT2,C'Y'          LIST ALL STATIONS & THEIR NETWORKS?          
         BNE   RREC200                                                          
*                                                                               
RREC30   MVC   P+2(5),STAKCALL                                                  
         MVC   P+8(3),=C':  '                                                   
         CLI   MYFLAG,C'Y'         WHICH REC LEN IS IT?                         
         BE    RREC100             REC LEN IS X'0160'                           
*                                                                               
***********************************************************************         
* STORE NETWORKS (FOR REC LEN X'0113')                                *         
***********************************************************************         
*                                                                               
         OC    SSYSNETS(16),SSYSNETS                                            
         BNZ   RREC40                                                           
         B     RREC200                                                          
*                                                                               
RREC40   MVC   NETWORKS,SSYSNETS                                                
         DROP  R6                                                               
*                                                                               
         SR    R0,R0               BIT COUNTER FOR NETWORKS                     
         LA    R2,NETWORKS         A(NETWORK LIST)                              
         SR    R6,R6               BIT IN THE CURRENT BYTE                      
         MVC   BYTE,0(R2)          GET FIRST BYTE OF NETWORKS                   
*                                                                               
RREC50   AH    R0,=H'1'            CHECK THIS NUMBERED BIT                      
         AH    R6,=H'1'                                                         
*                                                                               
         TM    BYTE,X'80'                                                       
         BZ    RREC70              THIS NETWORK NOT USED, CK NEXT ONE           
*                                                                               
         LA    RE,CABLETAB                                                      
RREC60   CLI   0(RE),X'FF'                                                      
         BE    RREC70                                                           
*                                                                               
RREC62   CLM   R0,1,3(RE)                                                       
         BNE   RREC65                                                           
*                                                                               
         ST    R1,SVREG1           SAVE NETWORK LISTED HERE INTO TABLE          
         L     R1,TABPTR           FOR LATER COMPARISON.                        
         MVC   0(3,R1),0(RE)                                                    
         MVI   3(R1),X'40'                                                      
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
         ST    R1,TABPTR                                                        
         L     R1,SVREG1                                                        
*                                                                               
         B     RREC70                                                           
*                                                                               
RREC65   LA    RE,L'CABLETAB(RE)   CHECK NEXT ENTRY IN NETWORK TABLE            
         B     RREC60                                                           
*                                                                               
RREC70   ZIC   R1,BYTE             SHIFT SO WE CAN   TM  BYTE,X'80'             
         SLL   R1,1                                                             
         STC   R1,BYTE                                                          
*                                                                               
         CH    R6,=H'8'            CHECKED ALL 8 BITS IN THIS BYTE?             
         BL    RREC50                                                           
*                                                                               
         LA    R2,1(R2)            YES, GET NEXT NETWORK BYTE                   
         MVC   BYTE,0(R2)                                                       
         SR    R6,R6                                                            
*                                                                               
         CH    R0,=H'127'          CHECK ALL POSSIBLE NETWORKS?                 
         BL    RREC50                                                           
         B     RREC200                                                          
*                                                                               
***********************************************************************         
* STORE NETWORKS (FOR REC LEN X'0160')                                *         
***********************************************************************         
RREC100  OC    WKTOP24,WKTOP24     ANY TOP 24 TO PRINT?                         
         BNZ   RREC110             YES                                          
         OC    WKCBLSEQ,WKCBLSEQ   ANY CABLE SEQ TO PRINT?                      
         BNZ   RREC150             YES                                          
*                                                                               
         B     RREC200                                                          
*                                                                               
RREC110  MVC   SVTOP24,WKTOP24                                                  
         LA    R3,SVTOP24          USING SAVED VERSION OF TOP 24                
         LA    R4,CABLETAB                                                      
*                                                                               
RREC120  TM    5(R4),X'40'         TOP 24 CABLE NETWORK?                        
         BZ    RREC130             NO, GET NEXT ITEM IN TABLE                   
*                                                                               
         TM    0(R3),X'80'         CURRENT TOP 24 BIT IS ON?                    
         BZ    RREC140             NO                                           
*                                                                               
         L     R1,TABPTR           FOR LATER COMPARISON.                        
         MVC   0(3,R1),0(R4)                                                    
         MVI   3(R1),X'40'                                                      
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
         ST    R1,TABPTR                                                        
*                                                                               
RREC140  ZICM  R5,SVTOP24,3                                                     
         SLL   R5,1                1 BIT LESS TO TEST                           
         STCM  R5,7,SVTOP24                                                     
         LA    R3,SVTOP24                                                       
*                                                                               
RREC130  LA    R4,L'CABLETAB(R4)                                                
         OC    SVTOP24,SVTOP24     STILL MORE TOP 24 BITS ARE ON?               
         BNZ   RREC120             YES, TEST SOME MORE                          
         OC    WKCBLSEQ,WKCBLSEQ   ANY CABLE SEQ TO PRINT?                      
         BNZ   RREC150             YES                                          
         B     RREC200                                                          
*                                                                               
RREC150  LA    R0,127              SET LOOP COUNT                               
         LA    R3,WKCBLSEQ                                                      
         LA    R4,CABLETAB                                                      
*                                                                               
RREC160  TM    5(R4),X'40'         TOP 24 CABLE NETWORK?                        
         BNZ   RREC190             YES, GET NEXT ITEM IN CABLE TABLE            
*                                                                               
RREC170  CLI   0(R3),X'00'                                                      
         BE    RREC190                                                          
         CLC   0(1,R3),3(R4)                                                    
         BNE   RREC180                                                          
*                                                                               
         L     R1,TABPTR           FOR LATER COMPARISON.                        
         MVC   0(3,R1),0(R4)                                                    
         MVI   3(R1),X'40'                                                      
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
         ST    R1,TABPTR                                                        
*                                                                               
         B     RREC190                                                          
*                                                                               
RREC180  LA    R3,1(R3)            NEXT ITEM IN CABLE SEQ                       
         B     RREC170                                                          
*                                                                               
RREC190  LA    R4,L'CABLETAB(R4)                                                
         LA    R3,WKCBLSEQ         REPOINT TO CABLE SEQ                         
         BCT   R0,RREC160                                                       
*                                                                               
         XC    WKTOP24,WKTOP24                                                  
         XC    SVTOP24,SVTOP24                                                  
         XC    WKCBLSEQ,WKCBLSEQ                                                
***********************************************************************         
*                                                                               
RREC200  LA    R1,OLDTAB                                                        
         ST    R1,TABPTR           POINT BACK TO BEGINNING OF TABLE             
         BAS   RE,COMPARE          COMPARE THE NEW TO OLD TABLE                 
         XCEFL OLDTAB,1000         CLEAR THE OLD TABLE                          
RREC300  BAS   RE,STASEQ           READ THE NEXT RECORD                         
         B     RREC10                                                           
*                                                                               
RRECX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* COMPARE: COMPARE OLDTAB AND NEWTAB TO SEE IF ANY NETWORK IS MISSING *         
***********************************************************************         
*                                                                               
COMPARE  NTR1                                                                   
         LA    R4,P+10                                                          
         LA    R5,118(R4)          CHECK END OF LINE                            
         LA    R2,NEWTAB           NETWORKS FROM DATASET                        
CP05     LA    R3,OLDTAB           NETWORKS FROM STATION FILE                   
         CLI   0(R2),X'FF'         ALL CROSS-CHECKED                            
         BE    CP50                                                             
CP10     CLI   0(R3),X'FF'                                                      
         BE    CP30                NOT FOUND                                    
         CLC   0(4,R2),0(R3)                                                    
         BE    CP20                FOUND                                        
         LA    R3,4(R3)                                                         
         B     CP10                KEEP CHECKING                                
*                                                                               
CP20     LA    R2,4(R2)            MATCH NEXT NETWORK                           
         B     CP05                                                             
*                                                                               
CP30     MVC   0(4,R4),0(R2)                                                    
         LA    R4,4(R4)                                                         
         CR    R4,R5                                                            
         BNH   CP40                                                             
         GOTO1 REPORT                                                           
         LA    R4,P+10                                                          
CP40     LA    R2,4(R2)                                                         
         B     CP05                                                             
*                                                                               
CP50     LA    R1,P+10                                                          
         CR    R4,R1                                                            
         BH    CP60                                                             
         MVC   P(10),SPACES                                                     
         B     XIT                                                              
CP60     AP    COUNTER1,=P'1'                                                   
         GOTO1 REPORT                                                           
         B     XIT                                                              
***********************************************************************         
*        NETCONV: CONVERT ALL 4 BYTE CODE TO 3 BYTES                  *         
***********************************************************************         
NETCONV  NTR1                                                                   
         LA    R1,NTWKCONV         NETWORK CODE CONVERSION                      
NETCV05  CLI   0(R1),0             END OF TABLE?                                
         BE    NETCV10                                                          
         CLC   SVNET,0(R1)                                                      
         BNE   *+14                                                             
         MVC   SVNET,4(R1)                                                      
         B     NETCV10                                                          
         LA    R1,8(R1)                                                         
         B     NETCV05                                                          
*                                                                               
NTWKCONV DC    XL4'C150C540',CL4'AE'       A&E                                  
         DC    CL4'ANPL',CL4'ANP'                                               
         DC    CL4'CH12',CL4'C12'                                               
         DC    CL4'E!TV',CL4'ETV'                                               
         DC    CL4'VH-1',CL4'VH1'                                               
         DC    CL4'ESP2',CL4'ES2'                                               
         DC    CL4'MTVL',CL4'MTL'                                               
         DC    CL4'INSP',CL4'ISP'                                               
         DC    CL4'SCNC',CL4'SCN'                                               
         DC    CL4'SCF ',CL4'SPF'                                               
         DC    CL4'SPSN',CL4'SSN'                                               
         DC    CL4'FOXA',CL4'FXA'                                               
         DC    CL4'CLAS',CL4'CSN'                                               
         DC    CL4'ODSY',CL4'ODY'                                               
         DC    CL4'SCNY',CL4'SPN'                                               
         DC    CL4'FFSO',CL4'FSO'                                               
         DC    CL4'FSW2',CL4'FS2'                                               
         DC    CL4'SCB ',CL4'SPB'                                               
         DC    CL4'SPH ',CL4'SPP'                                               
         DC    CL4'SCPC',CL4'CCP'                                               
         DC    CL4'SCNE',CL4'SPE'                                               
         DC    CL4'SCPF',CL4'SCA'                                               
         DC    CL4'ZON1',CL4'ZN1'                                               
         DC    CL4'ZON2',CL4'ZN2'                                               
         DC    CL4'ZON3',CL4'ZN3'                                               
         DC    CL4'ZON4',CL4'ZN4'                                               
         DC    CL4'ZON5',CL4'ZN5'                                               
         DC    CL4'ZON6',CL4'ZN6'                                               
         DC    CL4'FSNE',CL4'FNE'                                               
         DC    CL4'FSNY',CL4'FNY'                                               
         DC    CL4'FSO ',CL4'FXO'                                               
         DC    CL4'SPRE',CL4'SNP'                                               
         DC    CL4'CSNT',CL4'SNE'                                               
         DC    CL4'BAY9',CL4'BY9'                                               
         DC    CL4'COX3',CL4'CX3'                                               
         DC    CL4'M1CH',CL4'CH1'                                               
         DC    X'00'               END OF TABLE                                 
*                                                                               
NETCV10  MVI   SVNET+3,X'40'                                                    
         B     XIT                                                              
*                                                                               
***********************************************************************         
* CTFILE CALLS                                                                  
***********************************************************************         
CTLHIGH  MVC   COMMAND,DMRDHI                                                   
         B     CTLFILE                                                          
*                                                                               
CTLSEQ   MVC   COMMAND,DMRSEQ                                                   
*                                                                               
CTLFILE  MVC   DATADISP,=Y(CT5DATA-CT5REC)                                      
         ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'CTFILE',KEY,AREC               
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* STATION CALLS                                                                 
***********************************************************************         
STAHIGHD OI    DMINBTS,X'08'                                                    
STAHIGH  MVC   COMMAND,DMRDHI                                                   
         B     STAFILE                                                          
*                                                                               
STASEQ   MVC   COMMAND,DMRSEQ                                                   
         B     STAFILE                                                          
*                                                                               
STAADD   CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMADD                                                    
         B     STAFILE                                                          
*                                                                               
STAWRT   CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMWRT                                                    
*                                                                               
STAFILE  ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),STATION,KEY,AREC                  
         MVI   DMINBTS,0           ALWAYS CLEAR OUT DMINBTS                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
*                                                                               
***********************************************************************         
* SPTDIR CALLS                                                                  
***********************************************************************         
SPHIGHD  OI    DMINBTS,X'08'                                                    
SPHIGH   MVC   COMMAND,DMRDHI                                                   
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
*                                                                               
SPADD    CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMADD                                                    
         B     SPDIR                                                            
*                                                                               
SPWRT    CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMWRT                                                    
*                                                                               
SPDIR    ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),SPTDIR,KEY,KEY                    
         MVI   DMINBTS,0           ALWAYS CLEAR OUT DMINBTS                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
*                                                                               
***********************************************************************         
* SPTFILE CALLS                                                                 
***********************************************************************         
SPFGETR  MVC   COMMAND,GETREC                                                   
         B     SPFILE                                                           
*                                                                               
SPFADDR  CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,ADDREC                                                   
         B     SPFILE                                                           
*                                                                               
SPFPUTR  CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,PUTREC                                                   
*                                                                               
SPFILE   MVC   DATADISP,=Y(STADTAEL-STADDKEY)                                   
         ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,COMMAND,SPTFILE,KEY,AREC,DMWORK                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
*                                                                               
***********************************************************************         
* IN CASE RCWRITE=C'N', THEN ASSUME NO ERROR FROM DATAMGR                       
***********************************************************************         
DONTWRIT MVI   DMCB+8,0            MAKE IT SEEM LIKE WE HAVE NO ERROR           
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA)  OR  ZERO FOR ELEMENTAL RECORD                   
*        PARAMETER 3 - A(LABEL) OR  ZERO FOR NO LABEL                           
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=================================================================*             
MYTRACE  NTR1                                                                   
         CLI   QOPT1,C'Y'          OPTION SET TO DISPLAY TRACE?                 
         BNE   TRX                 NO                                           
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 PRNTBL,DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                         
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR100    DS    0H                                                               
*                                                                               
TRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VARIABLES AND STUFF                                                           
***********************************************************************         
CODTABLE DS    0XL2                                                             
         DC    C'D',AL1(CDNEWSYS)      NEW SYSTEM                               
         DC    C'E',AL1(CDPAYADR)      PAYABLE ADDRESS CHANGE                   
         DC    C'F',AL1(CDNETCHG)      NETWORK CHANGE                           
         DC    C'G',AL1(CDEXCLUS)      EXCLUSIVITY CHANGE                       
         DC    C'H',AL1(CDSYSNAM)      SYSTEM NAME CHANGE                       
         DC    C'I',AL1(CDMSOINT)      MSO AND INTERCONNECT CHANGE              
         DC    C'J',AL1(CDTPEADR)      TAPE ADDRESS CHANGE                      
         DC    C'X',AL1(CDNTACTV)      SYSTEM DEACTIVATED                       
         DC    X'00'                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         EJECT                                                                  
SPUPWRKD DS    0A                                                               
AREC1    DS    A                   A(FIRST RECORD AREA)                         
AREC2    DS    A                   A(SECOND RECORD AREA)                        
*                                                                               
SVDREGRE DS    A                   SAVED REGISTER RE                            
*                                                                               
COUNTER1 DS    PL8                 COUNTERS                                     
COUNTER2 DS    PL8                                                              
COUNTER3 DS    PL8                                                              
COUNTER4 DS    PL8                                                              
COUNTER5 DS    PL8                                                              
*                                                                               
BITFLAG1 DS    XL1                 FIRST SET OF BIT FLAGS                       
B1DBLQTE EQU   X'80'                   WE FOUND A DOUBLE QUOTE ALREADY          
B1COMMA  EQU   X'40'                   WE FOUND A COMMA ALREADY                 
B1NETWRK EQU   X'20'                   GOT NETWORK DATA                         
B1ACTION EQU   X'10'                   GOT ACTION CODE DATA                     
*                                                                               
BITFLAG2 DS    XL1                 SECOND SET OF BIT FLAGS                      
B2ADDING EQU   X'80'                   ADD RECORD (0=WRITE RECORD)              
*                                                                               
ACTNFLAG DS    XL1                 SAME BIT DEFINITION AS CODEFLAG              
CODEFLAG DS    XL1                 ACTION CODE BIT FLAG                         
CDNEWSYS EQU   X'80'                   'D' - NEW SYSTEM                         
CDPAYADR EQU   X'40'                   'E' - PAYABLE ADDRESS CHANGE             
CDNETCHG EQU   X'20'                   'F' - NETWORK CHANGE                     
CDEXCLUS EQU   X'10'                   'G' - EXCLUSIVITY CHANGE                 
CDSYSNAM EQU   X'08'                   'H' - SYSTEM NAME CHANGE                 
CDMSOINT EQU   X'04'                   'I' - MSO/INTERCONNECT CHANGE            
CDTPEADR EQU   X'02'                   'J' - TAPE ADDRESS CHANGE                
CDNTACTV EQU   X'01'                   'X' - SYSTEM DEACTIVATED                 
*                                                                               
ELCODE   DS    XL1                                                              
*                                                                               
ACTHDEND DS    CL5                 ACTION CABLE HEADEND (LAST USED)             
SYSHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
NETHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
CBLHDEND DS    CL5                 CABLE HEADEND (CURRENT)                      
SVDHDEND DS    CL5                 STATION CALL LETTERS                         
NUMMRKT  DS    CL4                 MKT NUM EBCDIC (BASED ON ALPHA MKT)          
PRVMRKT  DS    CL4                 PREVIOUS MARKET OF THE STATION               
TODAYDTE DS    CL6                 TODAY'S DATE EBCDIC YY/MM/DD                 
*                                                                               
SPTSENUM DS    XL1                 SPOT SENUM                                   
XCPTCLT  DS    CL3                 EXCEPTION CLIENT RECORD, 000 = REG.          
*                                                                               
NETWORKS DS    CL16                NETWORK BYTES                                
SVDNTWKS DS    CL16                SAVED NETWORK BYTES                          
*                                                                               
MYFLAG   DC    CL1'*'                                                           
WKTOP24  DC    XL3'00'             WORKING FIELD FOR TOP 24 NETWORKS            
SVTOP24  DC    XL3'00'             SAVED WORK FIELD OF TOP 24 NETWORKS          
WKCBLSEQ DC    XL64'00'            WORKING FIELD FOR CABLE SEQ NETWORKS         
*                                                                               
FILEIN   DCB   DDNAME=CBLTEST,MACRF=GM,DSORG=PS,RECFM=VB,BLKSIZE=4096, +        
               LRECL=4004,EODAD=NOMORE                                          
NEWTAB   DS    CL1000              TO STORE NETWORK CODES                       
OLDTAB   DS    CL1000                                                           
SVCALL   DS    CL4                                                              
SVNET    DS    CL4                                                              
SVKEY1   DS    XL15                                                             
SVREG1   DS    F                   RECYCLE REGISTER 1                           
TABPTR   DS    F                                                                
*                                                                               
NETLINE  DS    0CL4004                                                          
NETLN    DS    XL2                                                              
         DS    XL2                                                              
LINDATA  DS    CL4000                                                           
*                                                                               
SPOTREC  DS    CL4000                                                           
SPOTREC2 DS    CL4000                                                           
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
GENSTAD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
GENADDD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
         EJECT                                                                  
GENCBLD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
STARECX  EQU   *                   END OF TRAFFIC ADDRESS RECORD                
         EJECT                                                                  
*SPGENAGY                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
*SPGENANMK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENANMK                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
INPUTD   DSECT                      DSECT USED TO COVER THE INPUT LINE.         
INCALL   DS    CL4                                                              
         DS    CL4                                                              
INNTWK   DS    CL4                                                              
         DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089SPREPUP02S05/01/02'                                      
         END                                                                    
