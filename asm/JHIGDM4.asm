*          DATA SET JHIGDM4    AT LEVEL 091 AS OF 11/02/00                      
*PHASE JHIGDM4                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE UNTIME                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DEJAVU                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'YET ANOTHER DATAMGR EXAMPLE'                                    
JHIGDMGR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,JHIGDMGR,=V(REGSAVE),R9                                        
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(JHIGDMGR),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
* DON'T WORRY ABOUT THIS DMOPEN CALL                                            
*                                                                               
MAIN     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NGENDIR NGENFIL X',IO,0                                       
*                                                                               
* YOUR CODE GOES HERE                                                           
*                                                                               
         USING BRDKEYD,R4                                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                   BUILD KEY                              
         MVI   BRDKSYS,BRDKSYSQ                                                 
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVI   BRDKTYPE,BRDKTEMQ                                                
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,IO                
*                                                                               
LOOP     CLI   8(R1),0                   DIE IF NO RECORDS FOUND                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         CLC   KEY(BRDKMSGN-BRDKEY),BRDKEY                                      
         BNE   GOTALL                    EXIT WHEN KEY DOESN'T MATCH            
*                                                                               
         CLC   BRDKMSGN,=X'0000'         NO NOT ACCEPT NULL MSG NUMBER          
         BE    NEXT                                                             
*                                                                               
         ZIC   R2,BRDKSTAT+1                                                    
         SRL   R2,2                                                             
         CHI   R2,5                                                             
         BNE   NEXT                                                             
*                                                                               
         MVC   KEY,BRDKEY                SAVE OFF KEY                           
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'GETREC'),=C'GENFIL',             X        
               BRDDA,IO+8,DMWORK                                                
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO+8                                                          
         MVC   LEN,BRDRECLN              GET REC LENGTH                         
         XC    ZER,ZER                                                          
         MVI   ELCODE,X'10'              FIND '10' ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BRDFLTD,R4                                                       
         MVC   IO(L'BRDFNAME),BRDFNAME                                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTKEY                                  
         B     NEXT                                                             
*                                                                               
GOTALL   DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)                                                      
         BZ    GOODBYE                                                          
         BAS   RE,HEADING                                                       
         LA    R4,12(R4)                                                        
         LR    R3,R4                                                            
*                                                                               
         USING BRDKEYD,R4                                                       
         EDIT  BRDKMSGN,(5,P)            PUT MSG NUMBER ON PRINTLINE            
*                                                                               
         MVI   ELCODE,X'10'              FIND '10' ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BRDFLTD,R4                                                       
         USING FACITABD,R5                                                      
         LA    R5,FACIDTAB                                                      
TABLOOP  CLC   BRDFAPPL,FACIID           LOOK FOR ID NUMBER                     
         BE    GOTIT                                                            
         AHI   R5,L'FACITAB              BUMP TO NEXT TABLE ENTRY               
         B     TABLOOP                                                          
*                                                                               
GOTIT    DS    0H                                                               
         MVC   P+7(L'FACISN4),FACISN4                                           
*                                                                               
         USING CTRYTABD,R5                                                      
         LA    R5,CTRYTAB+6                                                     
         CLI   BRDFCTRY,X'FF'                                                   
         BE    ALLCTY                                                           
CTYLOOP  CLC   BRDFCTRY,CTRYCODE                                                
         BE    GOTCTY                                                           
         AHI   R5,L'CTRYTAB                                                     
         B     CTYLOOP                                                          
*                                                                               
GOTCTY   DS    0H                                                               
         MVC   P+14(L'CTRYNAM),CTRYNAM                                          
         B     *+10                                                             
*                                                                               
ALLCTY   MVC   P+14(3),=C'ALL'                                                  
*                                                                               
         OC    BRDFSTDT,BRDFSTDT                                                
         BZ    TIME                                                             
         GOTO1 =V(DATCON),DMCB,(2,BRDFSTDT),(11,SDATE)                          
         MVC   P+26(L'SDATE),SDATE                                              
*                                                                               
         OC    BRDFENDT,BRDFENDT                                                
         BZ    TIME                                                             
         GOTO1 =V(DATCON),DMCB,(2,BRDFENDT),(11,EDATE)                          
         MVC   P+36(L'EDATE),EDATE                                              
*                                                                               
TIME     DS    0H                                                               
         OC    BRDFSTTM,BRDFSTTM                                                
         BZ    DESC                                                             
         ZIC   RE,BRDFSTTM               START HOUR                             
         MHI   RE,100                                                           
         ZIC   RF,BRDFSTTM+1             START MINUTES                          
         AR    RE,RF                                                            
         STH   RE,FULL                   START TIME (MILITARY)                  
         ZIC   RE,BRDFENTM               END HOUR                               
         MHI   RE,100                                                           
         ZIC   RF,BRDFENTM+1             END MINUTES                            
         AR    RE,RF                                                            
         STH   RE,FULL+2                 END TIME (MILITARY)                    
         GOTO1 =V(UNTIME),DMCB,FULL,OUTTIME                                     
         MVC   P+47(L'OUTTIME),OUTTIME   PRINT TIMES                            
*                                                                               
DESC     MVC   P+60(L'BRDFNAME),BRDFNAME PRINT DESCRIPTION                      
*                                                                               
         MVI   DAYS,C' '                                                        
         MVC   DAYS+1(L'DAYS-1),DAYS                                            
         GOTO1 =V(DEJAVU),DMCB,(L'DAYS,BRDFDAYS),DAYS,0                         
         MVC   P+70(L'DAYS),DAYS                                                
*                                                                               
         USING BRDHEDD,R4                                                       
         LR    R4,R3                                                            
         MVI   ELCODE,X'20'              FIND '20' ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P+32(L'BRDHEDTX),BRDHEDTX                                        
*                                                                               
         USING BRDTXTD,R4                                                       
         LR    R4,R3                                                            
         MVI   ELCODE,X'30'              FIND '30' ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TXTLOOP  ZIC   R7,BRDTXTLN               GET LENGTH                             
         SHI   R7,8                      ADJUST LENGTH                          
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   P+6(0),BRDTXTTX                                                  
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,NEXTEL                                                        
         BE    TXTLOOP                                                          
*                                                                               
END      GOTO1 =V(PRINTER)                                                      
         B     GOTALL                                                           
*                                                                               
RESTORE  GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'GENDIR',KEY,IO                
*                                                                               
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'GENDIR',,IO                   
         B     LOOP                                                             
*                                                                               
GOODBYE  DS    0H                                                               
         XBASE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         EJECT                                                                  
*                                                                               
         GETEL R4,42,ELCODE                                                     
*                                                                               
HEADING  NTR1                                                                   
         MVC   P(44),=C'MSG #  SYS    COUNTRY     ST. DATE  END DATE'           
         MVC   P+47(28),=C'STIME ETIME   DESC      DAYS'                        
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                                     
*                                                                               
UTL      DC    F'0',X'0A'          FOR OFFLINE DATAMGR                          
*                                                                               
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
FULL     DS    F                                                                
ELCODE   DS    X                                                                
OUTTIME  DS    CL11                                                             
DAYS     DS    CL7                                                              
KEY      DS    CL32                GENDIR KEY                                   
WORK     DS    CL64                                                             
SDATE    DS    CL8                                                              
EDATE    DS    CL8                                                              
SORTKEY  DS    0CL2012                                                          
LEN      DS    XL2                                                              
ZER      DS    XL2                                                              
IO       DS    2008X               I/O AREA FOR GENFILE                         
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2008'                                  
SORTCARD DC    CL80'SORT FIELDS=(5,8,A),FORMAT=BI,WORK=1'                       
       ++INCLUDE FACIDTAB                                                       
       ++INCLUDE FACTRYTAB                                                      
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENBRD                                                       
         EJECT                                                                  
       ++INCLUDE FACIDTABD                                                      
         EJECT                                                                  
       ++INCLUDE FACTRY                                                         
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091JHIGDM4   11/02/00'                                      
         END                                                                    
