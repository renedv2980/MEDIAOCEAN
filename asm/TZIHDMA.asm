*          DATA SET TZIHDMA    AT LEVEL 057 AS OF 10/23/00                      
*PHASE TZIHDMA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'DATAMGR EXAMPLE'                                                
TZIHDMGR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,TZIHDMGR,=V(REGSAVE)                                           
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
         DC    A(TZIHDMGR),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
                                                                                
***********************************************************************         
MAIN     DS    0H                                                               
                                                                                
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'SPOT',               +        
               =C'NSTAFILEX',IOAREA,0                                           
         TM    DMCB+8,X'F2'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    KEY,KEY                                                          
                                                                                
         LA    R1,KEY                                                           
         USING STARECD,R1                                                       
                                                                                
* BUILD KEY TO SEARCH FOR STATION RECORDS                                       
         MVI   STAKTYPE,STAKTYPQ   TYPE=STATION                                 
         MVI   STAKMED,C'R'        MEDIA=RADIO                                  
                                                                                
         DROP  R1                                                               
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,IOAREA           
         TM    DMCB+8,X'F2'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
                                                                                
MLOOP    DS    0H                                                               
         CLI   8(R1),0             CHECK FOR ERRORS IN RETURN CODE              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,IOAREA                                                        
         USING STARECD,R2                                                       
                                                                                
* CHECK IF KEY STARTS WITH SR, IF NOT - END OF RADIO STATION RECORDS            
         CLC   KEY(STAKCALL-STAKEY),IOAREA                                      
         BNE   EMLOOP              END IF RECORD IS NOT WHAT WE NEED            
                                                                                
* CHECK IF IT IS OUR AGENCY                                                     
         CLC   STAKAGY,=C'SJ'                                                   
         BNE   NEXT                                                             
                                                                                
* CHECK BAND                                                                    
         CLC   =C'A',STAKCALL+4                                                 
         BNE   NEXT                                                             
                                                                                
* CHECK FOR EXCEPTION                                                           
         CLC   STAKCLT,=C'000'                                                  
         BNE   NEXT                                                             
                                                                                
*BUILD KEY BY WHICH RECORDS ARE TO BE SORTED                                    
         LA    R1,SRTKY                                                         
         USING SRTKYD,R1                                                        
                                                                                
         LHI   R0,SCBLSQNQ         LOAD RECORD LENGTH                           
         AHI   R0,SRTKYDLQ         ACCOUNT FOR SORT KEY,LENGTH                  
         STH   R0,SRECLEN                                                       
         MVC   SRECZRS,=2X'0'                                                   
                                                                                
         MVC   SCALL,STAKCALL                                                   
         MVC   SMCODE,SMKT                                                      
                                                                                
* SORT KEY DSECT OR STATION DSECT WON'T BE USED UNTIL PRINTING                  
         DROP  R1                                                               
         DROP  R2                                                               
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTKY                                    
                                                                                
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'STATION',KEY,IOAREA           
         TM    DMCB+8,X'F2'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         B     MLOOP                                                            
EMLOOP   DS    0H                                                               
                                                                                
         LA    R4,P                                                             
         USING PRD,R4                                                           
                                                                                
* PRINT THE TITLE LINE                                                          
         MVC   PKEY,=CL15'RECORD KEY'                                           
         MVC   PMCODE,=CL4'CODE'                                                
         MVC   PMNAME,=CL24'MARKET NAME'                                        
         MVC   PCALL,=CL5'NAME'                                                 
         GOTO1 =V(PRINTER)                                                      
         MVC   PKEY,=24C'-'                                                     
         MVC   PMCODE,=4C'-'                                                    
         MVC   PMNAME,=24C'-'                                                   
         MVC   PCALL,=5C'-'                                                     
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
                                                                                
*START RETRIEVING SORTED STATION RECORDS AND PRINTING THEM                      
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         XC    LASTMC,LASTMC                                                    
         XC    LASTMN,LASTMN                                                    
                                                                                
PLOOP    DS    0H                                                               
                                                                                
         ICM   R3,15,DMCB+4                                                     
         BZ    EPLOOP                                                           
                                                                                
         LA    R3,SRTKYDLQ(R3)     ADVANCE R3 TO BEGINNING OF STAT. REC         
         USING STARECD,R3                                                       
                                                                                
         CLC   LASTMC,SMKT                                                      
         BE    SKIPMKRD                                                         
                                                                                
* BUILD KEY TO SEARCH FOR MARKET NAME                                           
         LA    R1,KEY                                                           
         USING MKTRECD,R1                                                       
                                                                                
         MVI   MKTKTYPE,C'M'                                                    
         MVI   MKTKMED,C'R'                                                     
         MVC   MKTKMKT,SMKT                                                     
         MVC   MKTKAGY,=C'SJ'                                                   
         MVC   MKTKFILL,=7C'0'                                                  
                                                                                
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'STATION',KEY,IOAREA           
         TM    DMCB+8,X'F2'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
* NOTE, MARKET RECORD DSECT IS STILL USED OVER R1                               
         LA    R1,IOAREA           THIS IS WHERE MARKET RECORD IS               
                                                                                
         MVC   PMNAME,MKTNAME      MOVE MARKET NAME INTO PRINT AREA             
         MVC   LASTMN,MKTNAME                                                   
         MVC   LASTMC,MKTKMKT                                                   
                                                                                
SKIPMKRD DS    0H                                                               
         MVC   PMNAME,LASTMN                                                    
         MVC   PKEY(L'STAKEY),STAKEY                                            
         MVC   PMCODE(L'SMKT),SMKT                                              
         MVC   PCALL(L'STAKCALL),STAKCALL                                       
         GOTO1 =V(PRINTER)                                                      
                                                                                
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         B     PLOOP                                                            
EPLOOP   DS    0H                                                               
         DROP  R1                                                               
         DROP  R3                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
                                                                                
         XBASE                                                                  
         EJECT                                                                  
                                                                                
                                                                                
                                                                                
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                                     
*                                                                               
UTL      DC    F'0',X'02'          FOR OFFLINE DATAMGR                          
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
                                                                                
                                                                                
                                                                                
DMCB     DS    6F                                                               
KEY      DS    CL15                STAFILE KEY                                  
LASTMC   DS    CL4                                                              
LASTMN   DS    CL24                                                             
                                                                                
SRTKY    DS    CL(SRTKYDLQ)                                                     
IOAREA   DS    1000X               I/O AREA FOR STAFILE                         
                                                                                
*WORK     DS    (1000+SRTKYDLQ+4)X                                              
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=1200'                                  
SORTCARD DC    CL80'SORT FIELDS=(5,9,A),FORMAT=BI,WORK=1'                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
                                                                                
PRD      DSECT                                                                  
PMCODE   DS    CL4                                                              
         DS    CL(SPC)                                                          
PMNAME   DS    CL24                                                             
         DS    CL(SPC)                                                          
PCALL    DS    CL5                                                              
         DS    CL(SPC)                                                          
PKEY     DS    CL15                                                             
SPC      EQU   3                                                                
PRDLQ    EQU   *-PRD                                                            
                                                                                
SRTKYD   DSECT                                                                  
SRECLEN  DS    H                                                                
SRECZRS  DS    CL2                                                              
SMCODE   DS    CL4                                                              
SCALL    DS    CL5                                                              
SRTKYDLQ EQU   *-SRTKYD                                                         
                                                                                
                                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057TZIHDMA   10/23/00'                                      
         END                                                                    
