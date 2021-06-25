*          DATA SET DELMPAO    AT LEVEL 024 AS OF 11/01/11                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMPAOB                                                                 
         TITLE 'DEMCON - PAV CONVERSION - OUTPUT PHASE'                         
DECNVPAV CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PAVWRKDX-PAVWRKD,**PAVCNV                                        
         USING PAVWRKD,RC          RC=A(TEMP W/S)                               
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         USING DPRINT,R7           R7=A(PRINTER CSECT)                          
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD)                         
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNVX                LAST TIME HOOK                               
*                                                                               
CNV2     CLI   INTRTYP,PRCODEQU    RATINGS RECORDS                              
         BE    CNV6                                                             
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* BUILD DEMOGRAPHIC RECORDS                                                     
*                                                                               
CNV6     LA    R6,THISKEY                                                       
         USING PRKEY,R6                                                         
         XC    THISKEY,THISKEY                                                  
         MVI   PRCODE,PRCODEQU     BUILD KEY                                    
         MVC   PRMEDIA,MEDIA                                                    
         MVC   PRSRC,OUTSRC                                                     
         MVC   PRSTAT,INTSTA                                                    
         LA    RE,STYPTAB          CONVERT STATION TYPE                         
CNV6A    CLI   0(RE),X'FF'                                                      
         BE    CNV6B                                                            
         CLC   0(1,RE),INTSTYP                                                  
         BE    CNV6B                                                            
         LA    RE,L'STYPTAB(RE)                                                 
         B     CNV6A                                                            
CNV6B    MVC   PRSTAT+4(1),1(RE)   MOVE TYPE TO STATION                         
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   PRKMKT,INTMRKT                                                   
         MVC   PRBOOK,INTBOOK                                                   
*        MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
*        CLI   BOOKTYPE,0          ALLOW MANUAL OVERRIDE OF BTYP                
*        BE    *+10                                                             
*        MVC   PRBTYP,BOOKTYPE                                                  
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         CLC   PREVKEY(PRRLEN-PRKEY),PRKEY                                      
         BNE   CNV8                                                             
         ZIC   RE,PRDW             BUMP WEEK IF KEY EQ PREVKEY                  
         LA    RE,1(RE)                                                         
         STC   RE,PRDW                                                          
*                                                                               
CNV8     MVC   THISSQH,PRSTIM                                                   
         MVC   THISDW,PRDW                                                      
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         XC    TEMP,TEMP                                                        
         LA    R6,TEMP                                                          
         USING MARELEM,R6          BUILD MARKET TYPE ELEMENT                    
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT                                                    
         MVC   MARTYPE,INTMTYP                                                  
         MVC   MARSTYP,INTSTYP                                                  
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
*                                                                               
         XC    TEMP,TEMP                                                        
         USING PHELEM,R6           BUILD DAY/QTR HOUR ELEMENT                   
         MVI   PHCODE,PHCODEQ                                                   
         MVI   PHELN,PHELNEQ3                                                   
         MVI   PHREVID,X'FF'       REVISION ID FLAG (AS OF 10/93)               
         MVI   PHREV,3             REVISION NUMBER 3 (AS OF MAY/2011)           
         ZIC   RE,INTSQH                                                        
         ZIC   RF,INTEQH                                                        
         SR    RF,RE                                                            
         STC   RF,PHDUR                                                         
         MVC   PHDURTOT,INTADUR    ACTUAL NUMBER OR QUARTER HOURS               
*                                  IN THIS RECORD                               
         MVC   PHDURTO2,INTADUR2   ACTUAL NO. OF QUARTER-HOURS (2-BYTE)         
         MVC   PHPRVST,PREVSQH                                                  
         MVC   PHPRVDW,PREVDW                                                   
         MVC   PHDTYPE,INTDTYP                                                  
         MVC   PHDWKS,INTWEEKS                                                  
         MVC   PHDBOOK,INTBOOK                                                  
         MVC   PHPNUM3,INTPNUM+1   USE LAST THREE BYTES OF INTPNUM              
         MVC   PHPTYPE,INTPTYP     PROGRAM TYPE                                 
         MVC   PHPNAM6,INTPNAM6       "    NAME (6-CHAR VERSION)                
         MVC   PHPRSRC,INTPRSRC       "    SOURCE                               
         MVC   PHAFFIL,INTAFFL     NETWORK AFFILIATION                          
         MVC   PHNDAYS,INTNDAYS    NUMBER OF DAYS                               
         GOTO1 APUTEL,PHELEM                                                    
*                                                                               
         XC    TEMP,TEMP                                                        
         USING PPNELEM,R6          BUILD PROGRAM NAME ELEMENT                   
         MVI   PPNCODE,PPNCODEQ                                                 
         MVC   PPNNME(L'INTPNAME),INTPNAME                                      
         LA    R1,PPNNME+L'INTPNAME-1                                           
         CLI   0(R1),C' '          FIND L'NAME                                  
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         SR    R1,R6                                                            
         STC   R1,PPNELN                                                        
         GOTO1 APUTEL,PPNELEM                                                   
*                                                                               
         XC    TEMP,TEMP                                                        
         USING PDAELEM,R6          PROGRAM AIRED ELEMENT                        
         MVI   PDACODE,PDACODEQ                                                 
         MVI   PDAELN,PDALENQ                                                   
         MVC   PDADNUM,INTPDNUM                                                 
         MVC   PDADAYS,INTPDMAP                                                 
         GOTO1 APUTEL,PDAELEM                                                   
         DROP  R6                                                               
*                                  BUILD DEMO ELEMENTS                          
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'PAV'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,DRFRSTEL-DRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTBOOK                                                  
         ST    RA,DBCOMFCS                                                      
         MOVE  (CONDWORK,1000),INTACCS                                          
         GOTO1 CDEMEL,DMCB,(C'C',0),DBLOCKD,CONDWORK                            
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CONDWORK(2),CONDWORK                                             
         BZ    CNVX                                                             
         LA    R1,CONDWORK+2                                                    
         SR    R0,R0                                                            
*                                  ADD DEMO ELEMENTS TO RECORD                  
CNV10    CLI   0(R1),0                                                          
         BE    CNV12                                                            
         CLI   0(R1),X'5E'         CHECK FOR BOOK ELEMENT                       
         BNE   CNV10A                                                           
         CLC   INTBOOK,=AL2(JUL_82) IF BOOK IS LESS THAN OCT/82                 
         BH    *+14                                                             
         MVI   4(R1),C'N'          FORCE TO OLD PAV FORMAT - WHICH IS           
         MVC   5(2,R1),=X'5202'    HOW THE ELEMENTS ARE STUCTURED               
CNV10A   GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CNV10                                                            
*                                  PUT RECORD TO TAPE & SAVE VALUES             
CNV12    GOTO1 APUTTAPE                                                         
         MVC   PREVKEY,THISKEY                                                  
         MVC   PREVSQH,THISSQH                                                  
         MVC   PREVDW,THISDW                                                    
         B     CNVX                                                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                  TABLE OF STATION TYPES                       
STYPTAB  DS    0XL2                                                             
         DC    X'02',C'T'                                                       
         DC    X'01',C'1'                                                       
         DC    X'04',C'2'                                                       
         DC    X'22',C'T'                                                       
         DC    X'21',C'1'                                                       
         DC    X'24',C'2'                                                       
         DC    X'40',C'C'                                                       
         DC    X'FF',C'T'                                                       
         SPACE 2                                                                
*                                  THIS TIME VALUES                             
THISKEY  DC    XL20'00'                                                         
THISSQH  DC    X'00'                                                            
THISDW   DC    X'00'                                                            
*                                  LAST TIME VALUES                             
PREVKEY  DC    XL20'00'                                                         
PREVSQH  DC    X'00'                                                            
PREVDW   DC    X'00'                                                            
*                                                                               
SETACCL  DS    XL1                 SET INTACCS-DISPL. FLAG                      
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
PAVWRKD  DSECT                                                                  
CONDWORK DS    1000C                                                            
PAVWRKDX EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE DELMPINTD                                                      
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024DELMPAO   11/01/11'                                      
         END                                                                    
