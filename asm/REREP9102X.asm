*          DATA SET REREP9102X AT LEVEL 169 AS OF 08/21/97                      
*PHASE RE9102A,*                                                                
*INCLUDE SORTER                                                                 
         TITLE 'REREP9102 - RE9102 - STATION LEAVE/JOIN REPORT'                 
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP9102 --- STATION LEAVE/JOIN DATE REPORT               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 20AUG97 RHV -- VOILA!                                             *           
*                                                                   *           
*********************************************************************           
RE9102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STOREX-STORED,*RE9102*,R8,RR=R5                                  
         USING STORED,RC                                                        
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
                                                                                
         CLI   MODE,REQFRST        ONLY MODE SUPPORTED                          
         BE    REQF                                                             
                                                                                
         B     EXIT                                                             
                                                                                
NO       LTR   RB,RB               SET CONDITION CODES                          
         B     EXIT                                                             
YES      CR    RB,RB                                                            
EXIT     XMOD1                                                                  
                                                                                
**********************************************************************          
* REQFRST - PROCESS DOWNLOAD REPORT REQUEST                                     
**********************************************************************          
REQF     DS    0H                                                               
*                                                                               
         BAS   RE,GETREP           READ REP REC                                 
         BAS   RE,HEADSTA          STATION COLUMN HEADERS                       
         BAS   RE,DOSTA            DO STATION DOWNLOAD                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* HEADSTA - STATION COLUMN HEADERS                                              
**********************************************************************          
HEADSTA  NTR1                                                                   
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    EXIT                YES - NO HEADERS                             
         LA    R4,P                                                             
         USING STALINE,R4                                                       
         MVC   STAREP,=CL2'RP'                                                  
         MVC   STASTA,=CL7'STATION'                                             
         MVC   STAGRP,=CL10'GROUP'                                              
         MVC   STASUB,=CL10'SUBGROUP'                                           
         MVC   STAOWN,=CL20'OWNER'                                              
         MVC   STAAFF,=CL3'AFL'                                                 
         MVC   STAMKTCD,=CL4'MKT'                                               
         MVC   STAMKTNM,=CL20'MARKET NAME'                                      
         MVC   STATVB,=CL18'TVB REGION'                                         
         MVC   STARNK,=CL1'R'                                                   
         MVC   STAGL,=CL1'G'                                                    
         MVC   STADATE,=CL4'DATE'                                               
         BAS   RE,LOCALREP                                                      
         B     EXIT                                                             
         DROP  R4                                                               
**********************************************************************          
* GETREP - READ REP RECORD                                                      
**********************************************************************          
GETREP   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           SET KEY FOR REP RECORD                       
         MVC   KEY+25(2),QREP      INSERT REP CODE INTO KEY                     
         MVC   KEYSAVE(27),KEY     SAVE KEY FOR COMPARE                         
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                KEY MUST BE FOUND                            
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RREPREC,DMWORK                
*                                  RETRIEVE REP RECORD                          
GETRX    B     EXIT                                                             
*                                                                               
**********************************************************************          
* DOSTA - DO STATION DOWNLOAD                                                   
**********************************************************************          
DOSTA    NTR1                                                                   
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD   INITL SORTER                  
*                                                                               
         XC    SDATE(6),SDATE                                                   
         CLC   QSTART,SPACES       CUTOFF DATE?                                 
         BE    DOSTA02             NO                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(3,SDATE) YES - SAVE IT OFF               
         CLC   QEND,SPACES         CUTOFF DATE?                                 
         BNE   *+6                 NO                                           
         DC    H'0'                MUST HAVE BOTH DATES                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,EDATE) YES - SAVE IT OFF                 
*                                                                               
DOSTA02  DS    0H                                                               
         CLC   RREPMAST,=X'FFFF'   MASTER REP REQUEST?                          
         BE    DOSTA04             YES                                          
         MVC   REPCODE,RREPKREP    NO - RUN RPT FOR THIS REP CODE               
         B     DOSTA08                                                          
*                                                                               
DOSTA04  DS    0H                                                               
         LA    R6,RREPREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MASTER MUST HAVE SUBSIDIARIES                
         USING RREPSUB,R6                                                       
         ZIC   R7,RREPSCNT         SUBSIDIARY COUNT                             
         LA    R2,RREPSCOD         1ST SUBSIDIARY                               
         MVC   REPCODE,0(R2)       PROCESS 1ST SUBSID REP                       
         ST    R2,AREPS            SAVE PLACE IN SUBSID LIST                    
         B     DOSTA08                                                          
*                                                                               
DOSTA06  DS    0H                  INCREMENT PLACE IN SUBSID LIST               
         L     R2,AREPS                                                         
         LA    R2,2(R2)            NEXT SUBSID REP                              
         MVC   REPCODE,0(R2)                                                    
         ST    R2,AREPS                                                         
*                                                                               
DOSTA08  DS    0H                                                               
         XC    KEY,KEY             READ FIRST STA REC                           
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPCODE                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DOSTA12                                                          
DOSTA10  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         LA    R6,KEY                                                           
DOSTA12  DS    0H                                                               
         CLC   KEY(22),KEYSAVE     END OF STA REC'S?                            
         BNE   DOSTA100            YES  -DONE                                   
         CLI   RSTAKSTA+4,C'C'     COMBO PARENT?                                
         BE    DOSTA10             YES - SKIP                                   
         DROP  R6                                                               
         USING RSTAREC,IO2                                                      
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RSTAREC,DMWORK                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    QGROUP(2),SPACES                                                 
         CLI   QGROUP,C' '         GROUP FILTER?                                
         BZ    *+14                                                             
         CLC   RSTAGRUP(1),QGROUP                                               
         BNE   DOSTA10                                                          
*                                                                               
         CLI   QSBGROUP,C' '       SUBGROUP FILTER?                             
         BZ    *+14                                                             
         CLC   RSTAGRUP+1(1),QSBGROUP                                           
         BNE   DOSTA10                                                          
*                                                                               
         OI    QOPTION1,C' '                                                    
         CLI   QOPTION1,C' '       ACTIVE/INACTIVE FILTER?                      
         BE    DOSTA15                                                          
         CLI   QOPTION1,C'B'       BOTH?                                        
         BE    DOSTA15                                                          
         CLI   QOPTION1,C'A'       ACTIVE FILTER?                               
         BNE   DOSTA14                                                          
         OC    RSTAEND,RSTAEND                                                  
         BZ    DOSTA15             ACTIVE                                       
         B     DOSTA10             INACTIVE                                     
DOSTA14  CLI   QOPTION1,C'I'       INACTIVE FILTER?                             
         BE    *+6                                                              
         DC    H'0'                NO MORE CHOICES!                             
         OC    RSTAEND,RSTAEND                                                  
         BZ    DOSTA10             ACTIVE                                       
*                                                                               
DOSTA15  DS    0H                                                               
         OC    SDATE(6),SDATE      DATE FILTER?                                 
         BZ    DOSTA18             NO                                           
         OC    RSTAEND,RSTAEND     LEAVE DATE?                                  
         BZ    DOSTA16             NO                                           
         CLC   RSTAEND,SDATE       COMPARE DATES                                
         BL    DOSTA10                                                          
         CLC   RSTAEND,EDATE                                                    
         BH    DOSTA10                                                          
         B     DOSTA18                                                          
*                                                                               
DOSTA16  DS    0H                                                               
         OC    RSTASTRT,RSTASTRT   JOIN DATE?                                   
         BZ    DOSTA18                                                          
         CLC   RSTASTRT,SDATE                                                   
         BL    DOSTA10                                                          
         CLC   RSTASTRT,EDATE                                                   
         BH    DOSTA10                                                          
*                                                                               
DOSTA18  DS    0H                                                               
         LA    R4,P                                                             
         USING STALINE,R4                                                       
*                                                                               
         MVC   STASTA(4),RSTAKSTA  POINT TO 1ST SPACE AFTER LETTERS             
         LA    R5,STASTA                                                        
         LA    RE,4                                                             
         LA    R5,1(R5)                                                         
         OI    0(R5),C' '                                                       
         CLI   0(R5),C' '                                                       
         BE    *+8                                                              
         BCT   RE,*-16                                                          
*                                                                               
         MVI   0(R5),C'-'                                                       
         MVC   1(2,R5),=C'FM'                                                   
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    DOSTA20                                                          
         MVC   1(2,R5),=C'AM'                                                   
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    DOSTA20                                                          
         MVC   1(2,R5),=C'L '                                                   
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    DOSTA20                                                          
         MVC   1(2,R5),=C'TV'                                                   
         CLI   RSTAKSTA+4,C' '                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DOSTA20  DS    0H                                                               
         MVC   STAREP,RSTAKREP                                                  
         MVC   STAAFF,RSTAAFFL                                                  
         MVC   STARNK,RSTARANK                                                  
*                                                                               
DOSTA30  DS    0H                  ** READ TVB REGION TABLE **                  
         OC    RSTATVB,RSTATVB                                                  
         BZ    DOSTA40                                                          
         L     R5,=A(TVBLST)       TVB TABLE                                    
         B     *+8                                                              
DOSTA35  LA    R5,L'TVBLST(R5)                                                  
         CLI   0(R5),X'FF'         END?                                         
         BE    DOSTA40             DONE                                         
         CLC   RSTATVB,0(R5)       MATCH TVB CODE IN REC?                       
         BNE   DOSTA35                                                          
         MVC   STATVB,2(R5)        WRITE TVB NAME                               
*                                                                               
DOSTA40  DS    0H                                                               
         MVC   RESTKEY,RSTAKEY     SAVE KEY FOR RESTORE SEQ LOOP                
*                                                                               
         OC    RSTAOWN,RSTAOWN     ** READ OWNERSHIP RECORD **                  
         BZ    DOSTA50                                                          
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING ROWNREC,R6                                                       
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,RSTAKREP                                                
         MVC   ROWNKOWN,RSTAOWN                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BNE   DOSTA50                                                          
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         MVC   STAOWN,ROWNNAME                                                  
         DROP  R6                                                               
*                                                                               
DOSTA50  DS    0H                                                               
         OC    RSTAGRUP,RSTAGRUP   ** READ GROUP/SUBGROUP RECORD **             
         BZ    DOSTA55                                                          
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING RGRPREC,R6                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,RSTAKREP                                                
         MVC   RGRPKGRP,RSTAGRUP                                                
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BNE   DOSTA55                                                          
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         MVC   STAGRP,RGRPNAME                                                  
         MVC   STASUB,RGRPSBNM                                                  
         DROP  R6                                                               
*                                                                               
DOSTA55  DS    0H                  ** READ MARKET RECORD **                     
         LA    R6,RSTAREC          STATION '08' ELEM                            
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL            HAVE IT?                                     
         BNE   DOSTA60             NO - NO MKT CODE                             
         USING RSTAXXEL,R6                                                      
         OC    RSTAMKTC,RSTAMKTC   MARKET CODE?                                 
         BZ    DOSTA60             NO                                           
         MVC   STAMKTCD,RSTAMKTC   YES - WRITE IT OUT                           
         DROP  R6                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING RMKTREC,R6                                                       
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,RSTAKREP                                                
         MVC   RMKTKMKT,STAMKTCD                                                
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BNE   DOSTA60                                                          
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         MVC   STAMKTNM,RMKTNAME                                                
         DROP  R6                                                               
*                                                                               
DOSTA60  DS    0H                  GAIN/LOSS INDICATOR & DATES                  
         OC    RSTAEND,RSTAEND                                                  
         BZ    DOSTA70                                                          
         MVI   STAGL,C'L'                                                       
         GOTO1 DATCON,DMCB,(3,RSTAEND),(0,WORK)                                 
         MVC   STADATE(2),WORK+2                                                
         MVC   STADATE+2(2),WORK                                                
         B     DOSTA80                                                          
*                                                                               
DOSTA70  DS    0H                                                               
         OC    RSTASTRT,RSTASTRT                                                
         BZ    DOSTA80                                                          
         MVI   STAGL,C'G'                                                       
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(0,WORK)                                
         MVC   STADATE(2),WORK+2                                                
         MVC   STADATE+2(2),WORK                                                
*                                                                               
DOSTA80  DS    0H                                                               
         MVC   KEY,RESTKEY         ** RESTORE SEQ LOOP **                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',P                                        
         XC    P,P                                                              
         B     DOSTA10             NEXT STA REC                                 
         DROP  R4                                                               
*                                                                               
DOSTA100 DS    0H                                                               
         CLC   RREPMAST,=X'FFFF'         MASTER REP REQUEST?                    
         BNE   DOSTA110                  NO - DONE READING RECORDS              
         BCT   R7,DOSTA06                YES - BACK FOR NEXT REP                
*                                                                               
DOSTA110 DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'   GET FROM SORTER                        
         OC    DMCB+4(4),DMCB+4          NO MORE?                               
         BZ    EXIT                      ALL DONE - EXIT                        
         L     R3,4(R1)                                                         
         XC    P,P                                                              
         MVC   P,0(R3)                                                          
         BAS   RE,LOCALREP                                                      
         B     DOSTA110                                                         
*                                                                               
**********************************************************************          
* LOCAL REP - DOWNLOAD HANDLING                                                 
* ADEF MUST POINT TO APPROPRIATE DOWNLOAD DEFINITION CARD                       
**********************************************************************          
LOCALREP NTR1                                                                   
                                                                                
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
                                                                                
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         LA    R1,DEFSTA           R1 -> DOWNOAD DEFINITION                     
         DROP  R5                                                               
                                                                                
LOCAL010 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LOCAL020                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LOCAL010                                                         
                                                                                
LOCAL020 DS    0H                                                               
         MVI   LINE,1              NEVER PAGE BREAK                             
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
                                                                                
LOCALX   DS    0H                                                               
         B     EXIT                                                             
                                                                                
DEFSTA   DC    C'T',AL1(02)                                                     
         DC    C'T',AL1(07)                                                     
         DC    C'T',AL1(10)                                                     
         DC    C'T',AL1(10)                                                     
         DC    C'T',AL1(20)                                                     
         DC    C'T',AL1(03)                                                     
         DC    C'T',AL1(04)                                                     
         DC    C'T',AL1(20)                                                     
         DC    C'T',AL1(18)                                                     
         DC    C'T',AL1(01)                                                     
         DC    C'T',AL1(01)                                                     
         DC    C'T',AL1(04)                                                     
         DC    X'0000'                                                          
*                                                                               
       ++INCLUDE RETVBTAB          TVB CODES                                    
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(3,7,A,1,2,A),FORMAT=BI,WORK=1'                 
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=132'                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         DS    CL2000              INCREASE PROGRAM SIZE!!!!                    
*                                                                               
* WORKING STORAGE                                                               
*                                                                               
STORED   DSECT                                                                  
RELO     DS    A                                                                
ELCODE   DS    X                                                                
REPCODE  DS    CL2                 CURRENT REP CODE FOR STATION REPORT          
AREPS    DS    F                   A(CURRENT SUBSID REP) STATION RPT            
SDATE    DS    XL3                 START DATE YMD BINARY                        
EDATE    DS    XL3                 END DATE YMD BINARY                          
SVCLASS  DS    CL2                 SAVE ADV REC PRD CLASS                       
RESTKEY  DS    CL32                RESTORE SEQ LOOP KEY SAVE AREA               
SVMASTER DS    CL2                                                              
         DS    0D                                                               
IOAREA   DS    CL1000                                                           
IO2      DS    CL1000                                                           
STOREX   EQU   *                                                                
*                                                                               
STALINE  DSECT                                                                  
STAREP   DS    CL2                 REP CODE                                     
STASTA   DS    CL7                 STATION                                      
STAGRP   DS    CL10                GROUP                                        
STASUB   DS    CL10                SUBGROUP                                     
STAOWN   DS    CL20                OWNER                                        
STAAFF   DS    CL3                 AFFILIATE                                    
STAMKTCD DS    CL4                 MARKET CODE                                  
STAMKTNM DS    CL20                MARKET NAME                                  
STATVB   DS    CL18                TVB REGION                                   
STARNK   DS    CL1                 RANK                                         
STAGL    DS    CL1                 GAIN/LOSS                                    
STADATE  DS    CL4                 GAIN/LOSS DATE                               
STALEN   EQU   *-STALINE                                                        
*                                                                               
***********>LUDE RESUBREPS                                                      
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'169REREP9102X08/21/97'                                      
         END                                                                    
