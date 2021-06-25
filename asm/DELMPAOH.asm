*          DATA SET DELMPAOH   AT LEVEL 029 AS OF 10/01/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMPAHA                                                                 
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
         B     CNV20               PROCESS A RECORD                             
         B     CNVX                LAST TIME HOOK                               
*                                                                               
CNV20    CLI   INTRTYP,PRCODEQU    RATINGS RECORDS                              
         BE    CNV40                                                            
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* CONVERT STANDARD BOOKS TO NEW HUT/PUT BOOKS                                   
*                                                                               
CNV40    LA    RE,CNVBKTAB                                                      
CNV43    CLI   0(RE),X'FF'         END OF TABLE                                 
         BE    CNV50                                                            
         CLC   INTBTYP,0(RE)                                                    
         BE    CNV45                                                            
         LA    RE,2(RE)                                                         
         B     CNV43                                                            
CNV45    MVC   INTBTYP,1(RE)       MOVE IN NEW HUT/PUT BOOK                     
*                                                                               
* BUILD DEMOGRAPHIC RECORDS                                                     
*                                                                               
CNV50    LA    R6,THISKEY                                                       
         USING PRKEY,R6                                                         
         XC    THISKEY,THISKEY                                                  
         MVI   PRCODE,PRCODEQU     BUILD KEY                                    
         MVC   PRMEDIA,MEDIA                                                    
         MVC   PRSRC,OUTSRC                                                     
         MVC   PRSTAT,INTSTA                                                    
         LA    RE,STYPTAB          CONVERT STATION TYPE                         
CNV60    CLI   0(RE),X'FF'                                                      
         BE    CNV70                                                            
         CLC   0(1,RE),INTSTYP                                                  
         BE    CNV70                                                            
         LA    RE,L'STYPTAB(RE)                                                 
         B     CNV60                                                            
CNV70    MVC   PRSTAT+4(1),1(RE)   MOVE TYPE TO STATION                         
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
         BNE   CNV80                                                            
         ZIC   RE,PRDW             BUMP WEEK IF KEY EQ PREVKEY                  
         LA    RE,1(RE)                                                         
         STC   RE,PRDW                                                          
*                                                                               
CNV80    MVC   THISSQH,PRSTIM                                                   
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
CNV90    CLI   0(R1),0                                                          
         BE    CNV100                                                           
         CLI   0(R1),X'5E'         CHECK FOR BOOK ELEMENT                       
         BNE   CNV95                                                            
         CLC   INTBOOK,=AL2(JUL_82) IF BOOK IS LESS THAN OCT/82                 
         BH    *+14                                                             
         MVI   4(R1),C'N'          FORCE TO OLD PAV FORMAT - WHICH IS           
         MVC   5(2,R1),=X'5202'    HOW THE ELEMENTS ARE STUCTURED               
CNV95    GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CNV90                                                            
*                                  PUT RECORD TO TAPE & SAVE VALUES             
CNV100   GOTO1 APUTTAPE                                                         
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
*                                  CONVERT STANDARD BOOKS TYPES                 
*                                  TO TEMPORARY HUT/PUT BOOK TYPES              
*******CNVBKTAB DS    0XL2                                                      
         DC    X'D3',X'40'         CONVERT BOOK L TO BOOK ZL                    
         DC    X'E4',X'41'         CONVERT BOOK U TO BOOK ZU                    
         DC    X'E9',X'42'         CONVERT BOOK Z TO BOOK ZZ                    
         DC    X'D1',X'43'         CONVERT BOOK J TO BOOK ZJ                    
         DC    X'36',X'44'         CONVERT BOOK LS TO BOOK ZS                   
         DC    X'37',X'45'         CONVERT BOOK WS TO BOOK ZA                   
         DC    X'38',X'46'         CONVERT BOOK HS TO BOOK ZE                   
         DC    X'30',X'47'         CONVERT BOOK L3 TO BOOK Z3                   
         DC    X'31',X'48'         CONVERT BOOK C3 TO BOOK ZD                   
         DC    X'32',X'49'         CONVERT BOOK W3 TO BOOK ZB                   
         DC    X'3C',X'4A'         CONVERT BOOK H3 TO BOOK ZF                   
         DC    X'00',X'4B'         CONVERT BOOK NULL TO BOOK Z7                 
         DC    X'C3',X'4C'         CONVERT BOOK C TO BOOK ZC                    
         DC    X'E6',X'4D'         CONVERT BOOK W TO BOOK ZW                    
         DC    X'C8',X'4E'         CONVERT BOOK H TO BOOK ZH                    
         DC    X'FF'                                                            
         SPACE 2                                                                
CNVBKTAB DS    0XL2                                                             
         DC    AL1(BOOKTYPE_L),AL1(BOOKTYPE_ZL)                                 
         DC    AL1(BOOKTYPE_U),AL1(BOOKTYPE_ZU)                                 
         DC    AL1(BOOKTYPE_Z),AL1(BOOKTYPE_ZZ)                                 
         DC    AL1(BOOKTYPE_J),AL1(BOOKTYPE_ZJ)                                 
         DC    AL1(BOOKTYPE_LS),AL1(BOOKTYPE_ZS)                                
         DC    AL1(BOOKTYPE_WS),AL1(BOOKTYPE_ZA)                                
         DC    AL1(BOOKTYPE_HS),AL1(BOOKTYPE_ZE)                                
         DC    AL1(BOOKTYPE_L3),AL1(BOOKTYPE_Z3)                                
         DC    AL1(BOOKTYPE_C3),AL1(BOOKTYPE_ZD)                                
         DC    AL1(BOOKTYPE_W3),AL1(BOOKTYPE_ZB)                                
         DC    AL1(BOOKTYPE_H3),AL1(BOOKTYPE_ZF)                                
         DC    AL1(BOOKTYPE_STANDARD),AL1(BOOKTYPE_Z7)                          
         DC    AL1(BOOKTYPE_C),AL1(BOOKTYPE_ZC)                                 
         DC    AL1(BOOKTYPE_W),AL1(BOOKTYPE_ZW)                                 
         DC    AL1(BOOKTYPE_H),AL1(BOOKTYPE_ZH)                                 
         DC    X'FF'                                                            
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
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029DELMPAOH  10/01/13'                                      
         END                                                                    
