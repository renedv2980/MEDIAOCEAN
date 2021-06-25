*          DATA SET TZIHDM3C   AT LEVEL 077 AS OF 11/03/00                      
*PHASE TZIHDM3C                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE DEJAVU                                                                 
*INCLUDE SORTER                                                                 
                                                                                
         TITLE 'YET ANOTHER DATAMGR EXAMPLE'                                    
TZIHDM3C CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,TZIHDM3C,=V(REGSAVE),R9                                        
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
         DC    A(TZIHDM3C),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
                                                                                
*********************************************************************           
MAIN     DS    0H                                                               
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NGENDIR NGENFIL X',IOAREA,0                                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
* BUILD KEY TO SEARCH FOR BROADCAST MESSAGE RECORDS                             
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING BRDKEYD,R1                                                       
         MVI   BRDKSYS,BRDKSYSQ                                                 
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVI   BRDKTYPE,BRDKTEMQ                                                
         DROP  R1                                                               
                                                                                
         LA    R4,P                                                             
         USING PRD,R4                                                           
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,IOAREA            
                                                                                
         LHI   R0,BRDELDQ                                                       
         STH   R0,DATADISP                                                      
                                                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
                                                                                
MLOOP    DS    0H                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DS    H'0'                                                             
                                                                                
         CLC   IOAREA(10),KEY                                                   
         BNE   EMLOOP                                                           
                                                                                
         LA    R2,IOAREA                                                        
         USING BRDKEYD,R2                                                       
                                                                                
         CLC   BRDKMSGN(2),=X'0000'                                             
         BNH   MGETNEXT                                                         
                                                                                
         SR    R0,R0                                                            
         ZIC   R0,BRDKSTAT+1       3 BITS OF APPLICATION ID                     
         SRL   R0,2                                                             
         CHI   R0,5                                                             
         BNE   MGETNEXT                                                         
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'GETREC'),=C'GENFIL',BRDDA,IOAREA,+        
               DMWORK                                                           
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DS    H'0'                                                             
                                                                                
* TAKE CARE OF PRE-PENDED KEY FOR SORTER                                        
         XC    SORTPRE(12),SORTPRE                ZERO OUT PREFIX               
         SR    R0,R0                                                            
         ICM   R0,3,BRDRECLN                                                    
         AHI   R0,12                                                            
        STCM   R0,3,SORTPRE                                                     
                                                                                
* FIND ELEMENT X'10'                                                            
         MVI   ELCODE,BRDFLTCQ                                                  
         LA    R3,IOAREA                                                        
         USING BRDFLTD,R3                                                       
         BAS   RE,GETEL                                                         
         BNE   MNODESC                                                          
         MVC   SORTKY(8),BRDFNAME                                               
                                                                                
MNODESC  DS    0H                  NO DESCRIPTION, JUST PASS NULLS              
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTPRE                                  
                                                                                
         DROP  R3                  FILTER ELEMENT DSECT                         
         DROP  R2                  BROADCAST RECORD DSECT                       
                                                                                
MGETNEXT DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'GENDIR',KEY,IOAREA            
         B     MLOOP                                                            
EMLOOP   DS    0H                                                               
********************************************************************            
                                                                                
* ******** ***** ******* ******* ****** *                                       
* PRINTING PART, RECORDS ALREADY SORTED *                                       
* ******** ***** ******* ******* ****** *                                       
PLOOP    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,DMCB+4                                                     
         BZ    EPLOOP                                                           
                                                                                
         LA    R2,12(R2)                                                        
         USING BRDKEYD,R2                                                       
                                                                                
         MVC   PMSGN,=CL5'MSG #'                                                
         MVC   PCTRY,=CL11'COUNTRY'                                             
         MVC   PSDAT,=CL8'ST DATE'                                              
         MVC   PEDAT,=CL8'END DATE'                                             
         MVC   PDAYS,=CL20'DAYS'                                                
         MVC   PTIME,=CL11'TIME'                                                
         MVC   PDESC,=CL8'DESCR'                                                
         MVC   PHEAD,=CL40'HEADING'                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   PMSGN,=5C'-'                                                     
         MVC   PCTRY,=11C'-'                                                    
         MVC   PSDAT,=8C'-'                                                     
         MVC   PEDAT,=8C'-'                                                     
         MVC   PDAYS,=20C'-'                                                    
         MVC   PTIME,=11C'-'                                                    
         MVC   PDESC,=8C'-'                                                     
         MVC   PHEAD,=40C'-'                                                    
         GOTO1 =V(PRINTER)                                                      
                                                                                
         EDIT  (2,BRDKMSGN),(5,PMSGN)  TAKE CARE OF MESSAGE NO                  
                                                                                
         LR    R3,R2                                                            
         MVI   ELCODE,BRDFLTCQ                                                  
         BAS   RE,GETEL            GET FILTER ELEMENT                           
         BNE   MNOFILTR                                                         
                                                                                
         USING BRDFLTD,R3          R3 NOW POINTS AT FILTER ELEMENT              
                                                                                
* TAKE CARE OF COUNTRY CODE                                                     
         LA    R1,CTRYTAB                                                       
         USING CTRYTABD,R1                                                      
         ZIC   R0,BRDFCTRY         COUNTRY CODE                                 
         MH    R0,CTRYTAB          DISPLACEMENT INTO TABLE                      
         LA    R1,6(R1)            R1 ADVANCES TO FIRST ENTRY                   
         AR    R1,R0               POINTS TO COUNTRY                            
         MVC   PCTRY,CTRYNAM                                                    
         DROP  R1                                                               
                                                                                
* TAKE CARE OF START AND END DATES                                              
         CLC   BRDFSTDT(2),=2X'0'                                               
         BE    MSKP1                                                            
         GOTO1 =V(DATCON),DMCB,(2,BRDFSTDT),(11,PSDAT)                          
MSKP1    DS    0H                                                               
         CLC   BRDFENDT(2),=2X'0'                                               
         BE    MSKP2                                                            
         GOTO1 =V(DATCON),DMCB,(2,BRDFENDT),(11,PEDAT)                          
MSKP2    DS    0H                                                               
                                                                                
* TAKE CARE OF TIMES                                                            
         CLC   BRDFSTTM(4),=4X'0'                                               
         BE    MSKP3                                                            
         ZIC   RE,BRDFSTTM         START HOUR                                   
         MHI   RE,100                                                           
         ZIC   RF,BRDFSTTM+1       START MINUTES                                
         AR    RE,RF                                                            
         STH   RE,FULL             START TIME (MILITARY)                        
         ZIC   RE,BRDFENTM         END HOUR                                     
         MHI   RE,100                                                           
         ZIC   RF,BRDFENTM+1       END MINUTES                                  
         AR    RE,RF                                                            
         STH   RE,FULL+2           END TIME (MILITARY)                          
         GOTO1 =V(UNTIME),DMCB,FULL,PTIME                                       
MSKP3    DS    0H                                                               
                                                                                
*TAKE CARE OF DAYS IN DEJAVU FORMAT                                             
         GOTO1 =V(DEJAVU),DMCB,(20,BRDFDAYS),PDAYS,0                            
*TAKE CARE OF DESCRIPTION                                                       
         MVC   PDESC,BRDFNAME                                                   
                                                                                
         DROP  R3                                                               
                                                                                
         LR    R3,R2                                                            
         MVI   ELCODE,TVDHEDEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   MNOHEAD                                                          
         USING TVDHEDD,R3                                                       
         ZIC   R5,TVDHEDTL                                                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PHEAD(0),TVDHEDTX                                                
         DROP  R3                                                               
                                                                                
MNOHEAD  DS    0H                                                               
                                                                                
MNOFILTR DS    0H                                                               
                                                                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
                                                                                
         MVC   P(30),=CL30'M E S S A G E  T E X T'                              
         GOTO1 =V(PRINTER)                                                      
         MVI   P,C'='                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 =V(PRINTER)                                                      
*FOLLOWING LOOP SEARCHES FOR TEXT ELEMENTS                                      
         MVI   ELCODE,BRDTXTEQ                                                  
         LA    R3,IOAREA                                                        
         BAS   RE,GETEL                                                         
         USING BRDTXTD,R3                                                       
MTXTLP   DS    0H                                                               
         BNE   MENDTXT                                                          
         ZIC   R5,BRDTXTLN                                                      
         AHI   R5,-(BRDTXTOV)                                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BRDTXTTX                                                    
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,NEXTEL                                                        
         B     MTXTLP                                                           
MENDTXT  DS    0H                                                               
         MVI   P,C'='                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         DROP  R3                                                               
         DROP  R2                                                               
         DROP  R4                                                               
                                                                                
         B     PLOOP                                                            
EPLOOP   DS    0H                                                               
                                                                                
*********************************************************************           
         XBASE                                                                  
         EJECT                                                                  
                                                                                
         GETEL R3,DATADISP,ELCODE                                               
                                                                                
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
DMWORK   DS    12D                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
KEY      DS    CL32                GENDIR KEY                                   
WORK     DS    CL64                                                             
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2012'                                  
SORTCARD DC    CL80'SORT FIELDS=(5,8,A),FORMAT=BI,WORK=1'                       
       ++INCLUDE FACTRYTAB                                                      
       ++INCLUDE FACIDTAB                                                       
SORTPRE  DS    4X                                                               
SORTKY   DS    8C                                                               
IOAREA   DS    2000X               I/O AREA FOR GENFILE                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
PRD      DSECT                                                                  
PMSGN    DS    CL5                                                              
         DS    (SPC)C                                                           
PCTRY    DS    CL11                                                             
         DS    (SPC)C                                                           
PSDAT    DS    CL8                                                              
         DS    (SPC)C                                                           
PEDAT    DS    CL8                                                              
         DS    (SPC)C                                                           
PDAYS    DS    CL20                                                             
         DS    (SPC)C                                                           
PTIME    DS    CL11                                                             
         DS    (SPC)C                                                           
PDESC    DS    CL8                                                              
         DS    (SPC)C                                                           
PHEAD    DS    CL40                                                             
SPC      EQU   1                                                                
                                                                                
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENBRD                                                       
       ++INCLUDE FACIDTABD                                                      
       ++INCLUDE FACTRY                                                         
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077TZIHDM3C  11/03/00'                                      
         END                                                                    
