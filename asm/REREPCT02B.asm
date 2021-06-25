*          DATA SET REREPCT02B AT LEVEL 047 AS OF 05/01/02                      
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
* NOV17/95 (WSB) --- ORIGINAL CODING, COUNTS NUMBER OF RECORDS     *            
*                      UNDER EACH PHATOM REP FOR EACH RECORD TYPE  *            
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
         BAS   RE,GETREPS          BUILD TABLE OF ALL REPS                      
         BAS   RE,GETRECS          FIND THE NUMBER OF RECORDS PER REP           
         BAS   RE,PRTRECS          PRINT OUT THE RESULTS                        
         B     EXIT                EXIT                                         
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
******************************************************************              
INIT     NTR1                                                                   
         XC    GOODRECS,GOODRECS   NUMBER OF LEGIT REPS                         
         XC    PHANRECS,PHANRECS   NUMBER OF RECORDS WITH NO REP REC            
         LA    R3,REPTAB           CLEAR THE REP TABLE                          
         LA    R2,150              150 ENTRIES                                  
IN10     XC    0(L'REPTAB,R3),0(R3)                                             
         LA    R3,L'REPTAB(R3)                                                  
         BCT   R2,IN10                                                          
*                                                                               
         LA    R3,PHANTAB                                                       
         LA    R2,100                                                           
IN20     XC    0(L'PHANTAB,R3),0(R3)                                            
         LA    R3,L'PHANTAB(R3)                                                 
         BCT   R2,IN20                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*     BUILD A TABLE OF ALL THE REPS                                             
*                                                                               
GETREPS  NTR1                                                                   
         LA    R4,REPTAB           REP TABLE                                    
*                                                                               
         LA    R3,KEY              BUILD KEY FOR THE REP RECS                   
         USING RREPKEY,R3                                                       
*                                                                               
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKTYP,X'01'      X'01' IS REP RECORD                          
         GOTO1 HIGHDIR                                                          
*                                                                               
GREP10   CLC   KEYSAVE(1),KEY      STILL REP RECORDS?                           
         BNE   GREPX               NO                                           
*                                                                               
         MVC   0(2,R4),RREPKREP    PUT THE REP IN THE TABLE                     
*                                                                               
         LA    R4,L'REPTAB(R4)     GO TO NEXT                                   
         GOTO1 SEQDIR                                                           
         B     GREP10                                                           
*                                                                               
GREPX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*     FOR EACH ENTRY IN DISPTAB, READ ALL THE RECORDS IN REPDIR                 
*     WITH THAT RECORD TYPE.  FIND THE REP CODE IN THE RECORD BY                
*     USING THE DISPLACEMENT IN DISPTAB, FIND THAT REP IN REPTAB,               
*     AND INCREASE THE COUNT OF THE NUMBER OF RECORDS FOR THAT REP              
*                                                                               
GETRECS  NTR1                                                                   
         LA    R4,DISPTAB          GO THRU DISPTAB                              
GREC10   CLI   0(R4),X'00'         END OF TABLE?                                
         BE    GRECX               YES, DONE                                    
*                                                                               
         XC    KEY,KEY             NO, BUILD KEY FOR THIS RECORD TYPE           
         MVC   KEY(1),0(R4)        MOVE IN RECORD TYPE FROM DISPTAB             
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
         LA    R5,REPTAB           REP TABLE                                    
GREC30   CLC   0(2,R5),=X'0000'    WAS THIS REP IN THE TABLE?                   
         BNE   GREC45              COULD STILL BE                               
*                                                                               
         L     R3,PHANRECS         NO                                           
         LA    R3,1(R3)            ADD TO COUNT OF RECS WITH NO REP REC         
         ST    R3,PHANRECS                                                      
*                                                                               
         LA    R5,PHANTAB          ADDR OF PHANTOM TABLE                        
GREC35   CLC   0(2,R5),=X'0000'    END OF TABLE?                                
         BNE   GREC37              NO                                           
*                                                                               
         MVC   0(2,R5),THISREP     YES, PUT PHANTOM REP IN TABLE                
         LA    R3,1                                                             
         STCM  R3,15,2(R5)         ONE RECORD SO FAR                            
         B     GREC60                                                           
*                                                                               
GREC37   CLC   THISREP,0(R5)       IS THIS THE PHANTOM REP?                     
         BE    GREC40              YES                                          
*                                                                               
         LA    R5,6(R5)            NO, GO ON                                    
         B     GREC35                                                           
*                                                                               
GREC40   ICM   R3,15,2(R5)         COUNT OF NUM OF PHAN RECS FOR THIS           
         LA    R3,1(R3)                                                         
         STCM  R3,15,2(R5)                                                      
*                                                                               
         B     GREC60              CONTINUE ON                                  
*                                                                               
GREC45   CLC   THISREP,0(R5)       IS THIS THE PROPER REP?                      
         BE    GREC50              YES                                          
         LA    R5,6(R5)            NO, GO TO NEXT ENTRY                         
         B     GREC30                                                           
*                                                                               
GREC50   ICM   R3,15,2(R5)         COUNT OF NUMBER OF RECORDS FOR REP           
         LA    R3,1(R3)                                                         
         STCM  R3,15,2(R5)                                                      
*                                                                               
         L     R3,GOODRECS                                                      
         LA    R3,1(R3)            ADD TO COUNT OF LEGIT RECS                   
         ST    R3,GOODRECS                                                      
*                                                                               
GREC60   GOTO1 SEQDIR              CONTINUE WITH SAME RECORD TYPE               
         B     GREC20                                                           
*                                                                               
GRECNEXT BAS   RE,PRTPHAN                                                       
         LA    R4,2(R4)            LOOK FOR NEXT RECORD TYPE                    
         B     GREC10                                                           
*                                                                               
GRECX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*     GOES THROUGH PHANTAB AND PRINTS HOW MANY RECORDS THERE ARE FOR            
*     EACH PHANTOM REP WITH THIS RECORD TYPE.  ALSO CLEARS THE NUMBER           
*     FOR THE NEXT RECORD TYPE                                                  
*                                                                               
PRTPHAN  NTR1                                                                   
         MVI   PHANFLAG,C'N'       NO PHAN RECS YET                             
         LA    R4,PHANTAB                                                       
*                                                                               
PP10     CLC   0(2,R4),=X'0000'    END OF TABLE?                                
         BE    PP20                YES                                          
*                                                                               
         OC    2(4,R4),2(R4)       ANY FOR THIS PHAN REP?                       
         BZ    PP15                NO                                           
*                                                                               
         CLI   PHANFLAG,C'Y'                                                    
         BE    PP12                                                             
*                                                                               
         MVC   P+1(12),=C'RECORD TYPE '                                         
         GOTO1 HEXOUT,DMCB,KEYSAVE,P+13,1                                       
         GOTO1 REPORT                                                           
         MVI   PHANFLAG,C'Y'                                                    
*                                                                               
PP12     MVC   P+5(2),0(R4)        PRINT THE PHAN REP                           
         EDIT  (4,2(R4)),(10,P+10) PRINT NUMBER OF RECORDS WITH IT              
         GOTO1 REPORT                                                           
         XC    2(4,R4),2(R4)       CLEAR OUT FOR NEXT REC TYPE                  
PP15     LA    R4,6(R4)            GO TO NEXT ENTRY                             
         B     PP10                                                             
*                                                                               
PP20     CLI   PHANFLAG,C'Y'                                                    
         BNE   PPX                                                              
         GOTO1 REPORT                                                           
*                                                                               
PPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
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
         MVC   P+6(31),=C'NUMBER OF LEGITIMATE RECORDS:  '                      
         EDIT  GOODRECS,(10,P+37),ZERO=NOBLANK                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+6(28),=C'NUMBER OF PHANTOM RECORDS:  '                         
         EDIT  PHANRECS,(10,P+34),ZERO=NOBLANK                                  
         GOTO1 REPORT                                                           
*                                                                               
         L     R3,GOODRECS                                                      
         A     R3,PHANRECS                                                      
         MVC   P+6(26),=C'TOTAL NUMBER OF RECORDS:  '                           
         EDIT  (R3),(10,P+32),ZERO=NOBLANK                                      
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
GOODRECS DS    F                                                                
PHANRECS DS    F                                                                
PHANFLAG DS    C                                                                
*                                                                               
REPTAB   DS    150CL6              2 BYTES REP CODE, 4 OF NUM OF RECS           
PHANTAB  DS    100CL6              **** ADDED                                   
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
**PAN#1  DC    CL21'047REREPCT02B05/01/02'                                      
         END                                                                    
