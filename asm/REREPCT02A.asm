*          DATA SET REREPCT02A AT LEVEL 042 AS OF 05/01/02                      
*PHASE RECT02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPCT02 (RECT02) - REP RECORDS COUNTING'                      
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPCT02 -- READ ENTIRE REP FILE AND REPORT RECORDS      *            
*                      UNDER INDIVIDUAL REP CODES                               
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* NOV11/95 (SKU) --- ORIGINAL ENTRY                                *            
*                                                                  *            
* NOV17/95 (WSB) --- ORIGINAL CODING, COUNTS NUMBER RECORDS OF     *            
*                      EACH TYPE UNDER REP 'MG'                    *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RECT02   CSECT                                                                  
         NMOD1 0,**RECT**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE THE STORAGE                       
         BAS   RE,GETRECS          FIND THE NUMBER OF RECORDS PER REP           
         B     EXIT                EXIT                                         
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
******************************************************************              
INIT     NTR1                                                                   
         XC    NONREPS,NONREPS     NUMBER OF RECORDS WITH NO REP REC            
         LA    R3,REPTAB           CLEAR THE REP TABLE                          
         LA    R2,150              150 ENTRIES                                  
IN10     XC    0(L'REPTAB,R3),0(R3)                                             
         LA    R3,L'REPTAB(R3)                                                  
         BCT   R2,IN10                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*     BUILD A TABLE OF ALL THE REPS                                             
*                                                                               
GETREPS  NTR1                                                                   
         LA    R4,REPTAB           REP TABLE                                    
*                                                                               
*        LA    R3,KEY              BUILD KEY FOR THE REP RECS                   
*        USING RREPKEY,R3                                                       
*                                                                               
*        XC    RREPKEY,RREPKEY                                                  
*        MVI   RREPKTYP,X'01'      X'01' IS REP RECORD                          
*        GOTO1 HIGHDIR                                                          
*                                                                               
*REP10   CLC   KEYSAVE(1),KEY      STILL REP RECORDS?                           
*        BNE   GREPX               NO                                           
*                                                                               
         MVC   0(2,R4),=C'MG'      PUT THE REP IN THE TABLE                     
*                                                                               
*        LA    R4,L'REPTAB(R4)     GO TO NEXT                                   
*        GOTO1 SEQDIR                                                           
*        B     GREP10                                                           
*                                                                               
GREPX    B     EXIT                                                             
*        DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*     FOR EACH ENTRY IN DISPTAB, READ ALL THE RECORDS IN REPDIR                 
*     WITH THAT RECORD TYPE.  FIND THE REP CODE IN THE RECORD BY                
*     USING THE DISPLACEMENT IN DISPTAB, FIND THAT REP IN REPTAB,               
*     AND INCREASE THE COUNT OF THE NUMBER OF RECORDS FOR THAT REP              
*                                                                               
GETRECS  NTR1                                                                   
         LA    R2,0                TOTAL NUM OF MG'S                            
         LA    R4,DISPTAB          GO THRU DISPTAB                              
GREC10   CLI   0(R4),X'00'         END OF TABLE?                                
         BE    GRECX               YES, DONE                                    
*                                                                               
         XC    KEY,KEY             NO, BUILD KEY FOR THIS RECORD TYPE           
         MVC   KEY(1),0(R4)        MOVE IN RECORD TYPE FROM DISPTAB             
         LA    R7,0                NUM OF MG'S FOR THIS REC TYPE                
         GOTO1 HIGHDIR                                                          
*                                                                               
GREC20   CLC   KEYSAVE(1),KEY      STILL THIS RECORD TYPE?                      
         BNE   GRECNEXT            NO                                           
*                                                                               
         LA    R3,KEY              ADDRESS OF RECORD                            
         ZIC   R5,1(R4)            DISPLACEMENT OF REP INTO THE RECORD          
         AR    R3,R5               THE REP ITSELF                               
         MVC   THISREP,0(R3)                                                    
*                                                                               
         CLC   THISREP,=C'MG'                                                   
         BNE   GREC60              CONTINUE ON                                  
*                                                                               
         LA    R2,1(R2)                                                         
         LA    R7,1(R7)                                                         
*                                                                               
GREC60   GOTO1 SEQDIR              CONTINUE WITH SAME RECORD TYPE               
         B     GREC20                                                           
*                                                                               
GRECNEXT GOTO1 HEXOUT,DMCB,0(R4),P+1,1                                          
         EDIT  (R7),(10,P+5)                                                    
         GOTO1 REPORT                                                           
         LA    R4,2(R4)            LOOK FOR NEXT RECORD TYPE                    
         B     GREC10                                                           
*                                                                               
GRECX    MVC   P+6(15),=C'TOTAL NUMBER:  '                                      
         EDIT  (R2),(10,P+21)                                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*     GOES THROUGH REPTAB AND PRINTS OUT HOW MANY RECORDS ARE                   
*     ASSOCIATED WITH EACH REP.  ALSO PRINTS HOW MANY RECORDS HAVE              
*     NO REP REC                                                                
*                                                                               
PRTRECS  NTR1                                                                   
         LA    R4,REPTAB                                                        
*                                                                               
PR10     CLC   0(2,R4),=X'0000'    END OF TABLE?                                
         BE    PR20                YES                                          
         MVC   P+1(2),0(R4)        PRINT THE REP                                
         EDIT  (4,2(R4)),(10,P+6)  PRINT NUMBER OF RECORDS WITH IT              
         GOTO1 REPORT                                                           
         LA    R4,6(R4)            GO TO NEXT ENTRY                             
         B     PR10                                                             
*                                                                               
PR20     GOTO1 REPORT                                                           
         MVC   P+6(36),=C'NUMBER OF RECORDS WITH NO REP REC:  '                 
         EDIT  NONREPS,(10,P+42),ZERO=NOBLANK                                   
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
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
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
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
         B     EXIT                                                             
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     EXIT                                                             
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
         B     EXIT                                                             
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ABLDAREA DS    A                                                                
AGRPAREA DS    A                                                                
ANEXTGRP DS    A                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
ACOMAREA DS    A                                                                
ANEXTCOM DS    A                                                                
ASTNAREA DS    A                                                                
LBLDAREA DS    F                                                                
NEXTAREA DS    A                   NEXT OPEN SLOT                               
STRTSRCH DS    A                   A(START OF SEARCH)                           
NUMBLD   DS    F                                                                
NUMCONS  DS    F                                                                
NUMBUYS  DS    F                                                                
CONBYTES DS    F                                                                
BUYBYTES DS    F                                                                
CONCTR   DS    F                                                                
BUYCTR   DS    F                                                                
ADVCTR   DS    F                                                                
SALCTR   DS    F                                                                
PRDCTR   DS    F                                                                
OTHERCTR DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
NEWREP   DC    CL2'KH'             NEW REP CODE                                 
FLAGBYTS DS    0CL12               FLAGS                                        
BOTHOPEN DS    CL1                 NEITHER STATION LEFT                         
HNOPEN   DS    CL1                                                              
DIOPEN   DS    CL1                                                              
BOTHLEFT DS    CL1                                                              
         DS    CL8                 SPARE                                        
REPCODE  DS    CL2                                                              
KEYTYPE  DS    CL1                                                              
DBLSPACE DS    CL1                                                              
DATEWORK DS    CL24                DATE WORK AREA                               
SAVEKEY  DS    CL(L'KEY)                                                        
*                                                                               
ELCODE   DS    CL1                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
THISREP  DS    CL2                                                              
NONREPS  DS    F                                                                
*                                                                               
REPTAB   DS    150CL6              2 BYTES REP CODE, 4 OF NUM OF RECS           
*                                                                               
DISPTAB  DC    X'02',AL1(20)       RECORD TYPE, DISPL OF REP INTO REC           
         DC    X'03',AL1(23)                                                    
         DC    X'04',AL1(23)                                                    
         DC    X'44',AL1(23)                                                    
         DC    X'05',AL1(23)                                                    
         DC    X'06',AL1(22)                                                    
         DC    X'07',AL1(23)                                                    
         DC    X'08',AL1(25)                                                    
         DC    X'09',AL1(25)                                                    
         DC    X'0A',AL1(25)                                                    
         DC    X'1A',AL1(25)                                                    
         DC    X'0B',AL1(16)                                                    
         DC    X'0C',AL1(2)                                                     
         DC    X'0D',AL1(23)                                                    
         DC    X'0F',AL1(23)                                                    
         DC    X'11',AL1(6)                                                     
         DC    X'12',AL1(10)                                                    
         DC    X'13',AL1(16)                                                    
         DC    X'14',AL1(17)                                                    
         DC    X'16',AL1(14)                                                    
         DC    X'18',AL1(24)                                                    
         DC    X'19',AL1(17)                                                    
         DC    X'1B',AL1(15)                                                    
         DC    X'1C',AL1(13)                                                    
         DC    X'1D',AL1(17)                                                    
         DC    X'1E',AL1(16)                                                    
         DC    X'22',AL1(13)                                                    
         DC    X'23',AL1(23)                                                    
         DC    X'24',AL1(24)                                                    
         DC    X'26',AL1(20)                                                    
         DC    X'27',AL1(1)                                                     
         DC    X'28',AL1(13)                                                    
         DC    X'29',AL1(11)                                                    
         DC    X'2A',AL1(22)                                                    
         DC    X'2B',AL1(21)                                                    
         DC    X'2C',AL1(4)                                                     
         DC    X'2D',AL1(12)                                                    
         DC    X'2E',AL1(15)                                                    
         DC    X'30',AL1(17)                                                    
         DC    X'31',AL1(22)                                                    
         DC    X'32',AL1(24)                                                    
         DC    X'33',AL1(17)                                                    
         DC    X'34',AL1(20)                                                    
         DC    X'35',AL1(13)                                                    
         DC    X'36',AL1(17)                                                    
         DC    X'37',AL1(13)                                                    
         DC    X'38',AL1(19)                                                    
         DC    X'39',AL1(13)                                                    
         DC    X'3A',AL1(22)                                                    
         DC    X'3B',AL1(23)                                                    
         DC    X'3C',AL1(24)                                                    
         DC    X'41',AL1(7)                                                     
         DC    X'00'                                                            
*                                                                               
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
*                                                                               
*  INCLUDE REGENCOM                COMMISSION RECORD                            
*  INCLUDE REGENREG                REGION RECORD                                
*  INCLUDE REGENOFF                OFFICE RECORD                                
*  INCLUDE REGENEOM                EOM RECORD                                   
*  INCLUDE REGENDPT                DAYPART RECORD                               
*  INCLUDE REGENBUD                BUDGET RECORD                                
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION RECORD                               
*  INCLUDE REGENSDD                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         PRINT OFF                                                              
         ORG   RECORD                                                           
       ++INCLUDE REGENCOM          COMMISSION RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREG          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOFF          OFFICE     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOM          END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDPTA         END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUD          BUDGET RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCLS          CLASS RECORD                                 
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTG          CATEGORY RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTY          K TYPE RECORD                                
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE REGENREPA                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042REREPCT02A05/01/02'                                      
         END                                                                    
