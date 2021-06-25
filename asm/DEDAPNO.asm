*          DATA SET DEDAPNO    AT LEVEL 016 AS OF 10/18/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEDAPNOA                                                                 
         TITLE 'DEMCON - LOCAL DAILIES PROGRAM NAME OPHASE'                     
DEDAPNO  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PAVWRKDX-PAVWRKD,**DAPNO                                         
         USING PAVWRKD,RC          RC=A(TEMP W/S)                               
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         USING DPRINT,R7           R7=A(PRINTER CSECT)                          
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD)                         
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV10               PROCESS A RECORD                             
         B     CNVX                LAST TIME HOOK                               
*                                                                               
CNV10    CLI   INTRTYP,PRCODEQU    RATINGS RECORDS                              
         BE    CNV20                                                            
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* BUILD DEMOGRAPHIC RECORDS                                                     
*                                                                               
CNV20    LA    R6,THISKEY                                                       
         USING PRKEY,R6                                                         
         XC    THISKEY,THISKEY                                                  
         MVI   PRCODE,PRCODEQU     BUILD KEY                                    
         MVC   PRMEDIA,MEDIA                                                    
         MVI   PRMEDIA,C'O'                                                     
         MVC   PRSRC,OUTSRC                                                     
         MVC   PRSTAT,INTSTA                                                    
         LA    RE,STYPTAB          CONVERT STATION TYPE                         
CNV30    CLI   0(RE),X'FF'                                                      
         BE    CNV40                                                            
         CLC   0(1,RE),INTSTYP                                                  
         BE    CNV40                                                            
         LA    RE,L'STYPTAB(RE)                                                 
         B     CNV30                                                            
CNV40    MVC   PRSTAT+4(1),1(RE)   MOVE TYPE TO STATION                         
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   PRKMKT,INTMRKT                                                   
*                                                                               
         MVI   AMFLG,C'N'          NON 2AM MKTS GOES FROM 3A-245A               
         MVI   LATEN,84                                                         
         MVI   DAYX,83                                                          
*                                                                               
         LA    RE,AMTAB                                                         
CNV41    CLI   0(RE),X'FF'                                                      
         BE    CNV45                                                            
         CLC   INTMRKT,0(RE)                                                    
         BNE   *+20                                                             
         MVI   AMFLG,C'Y'                                                       
         MVI   LATEN,80            2AM MARKETS GOES FROM 2A-145A                
         MVI   DAYX,79                                                          
         B     CNV45                                                            
         LA    RE,L'AMTAB(RE)                                                   
         B     CNV41                                                            
*                                                                               
CNV45    DS    0H                                                               
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         CLI   BOOKTYPE,0          ALLOW MANUAL OVERRIDE OF BTYP                
         BE    *+10                                                             
         MVC   PRBTYP,BOOKTYPE                                                  
         MVC   PRSTIM,INTEQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         MVI   PRBTYP+1,C'P'-X'40' SUB BOOK TYPE                                
         MVC   PRBTYP+2(1),PRDW                                                 
*                                                                               
         CLC   PREVKEY(PRDAYWK-PRKEY+1),THISKEY                                 
         BE    *+8                 MAJOR KEY CHANGED                            
         BAS   RE,RELPTR                                                        
*                                                                               
*        CLI   INTSQH,LATED        MAKE SURE WE COVER EARLY & LATE              
*        BH    CNV48                                                            
*        CLI   PREVDPT,C'E'        THIS ONE'S AN EARLY ONE                      
*        BE    CNV50               SO WAS PREVIOUS: OK                          
*        BAS   RE,RELPTR           PREVIOUS LATE?:  RELEASE EARLY PTR           
*                                                                               
*NV48    DS    0H                                                               
*        CLI   PREVDPT,C'E'        THIS ONE'S A LATE ONE                        
*        BNE   CNV50               SO WAS PREVIOUS: OK                          
*        CLI   INTSQH,X'5B'        5AM IS ACTUALLY EARLY.                       
*        BH    CNV50                                                            
*        BAS   RE,RELPTR           PREVIOUS EARLY:  RELEASE LATE PTR            
*                                                                               
CNV50    MVC   THISSQH,PRSTIM                                                   
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
         MVI   PHELN,PHELNEQ2                                                   
         MVI   PHPNUM,X'FF'        REVISION ID FLAG (AS OF 10/93)               
         MVI   PHPNUM+1,2          REVISION NUMBER (AS OF MARCH/2000)           
         ZIC   RE,INTSQH                                                        
         ZIC   RF,INTEQH                                                        
         CR    RF,RE                                                            
         BNL   *+8                                                              
         AHI   RF,96                                                            
         SR    RF,RE                                                            
         STC   RF,PHDUR                                                         
*        MVC   PHSTIM,INTSQH                                                    
*        MVC   PHSDAY,INTDAYWK                                                  
         MVC   PHDURTOT,INTADUR    ACTUAL NUMBER OR QUARTER HOURS               
*                                  IN THIS RECORD                               
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
*                                  PUT RECORD TO TAPE & SAVE VALUES             
         XC    TEMP,TEMP                                                        
         USING PTTVELEM,R6                                                      
         MVI   PTTVCODE,PTTVCDEQ                                                
         MVC   PTTVTRK,INTTRKID                                                 
         MVC   PTTVTEL,INTTELID                                                 
         MVC   PTTVVAR,INTVARID                                                 
         MVI   PTTVLN,PTTVLNQ                                                   
         GOTO1 APUTEL,PTTVELEM                                                  
*                                  PUT RECORD TO TAPE & SAVE VALUES             
CNV80    GOTO1 APUTTAPE                                                         
         MVC   PREVKEY,THISKEY                                                  
         MVC   PREVSQH,THISSQH                                                  
         MVC   PREVDW,THISDW                                                    
         B     CNVX                                                             
         EJECT                                                                  
*                                                                               
RELPTR   NTR1                                                                   
         LA    R6,THISKEY                                                       
         USING PRKEY,R6                                                         
         LA    R5,TEMP                                                          
         USING MRGRELEM,R5                                                      
         MVI   MRGRCODE,MRGRCDEQ                                                
         MVI   MRGRLEN,MRGRLENQ                                                 
         MVI   MRGRTYPE,MRGRLOW                                                 
         MVC   MRGREYEC,MRGEYEC                                                 
*                                                                               
         MVC   SAVEKEY,THISKEY                                                  
         CLC   INTSQH,LATEN                                                     
         BL    RP05                                                             
         CLC   INTSQH,LATENX                                                    
         BNH   RP10                                                             
*                                                                               
RP05     MVC   PRSTIM,DAY                                                       
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
         MVC   PRSTIM,DAYX                                                      
         MVI   MRGRTYPE,MRGRHIGH                                                
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
*                                                                               
         MVI   MRGRTYPE,MRGRLOW                                                 
         MVC   PRSTIM,EARLYM                                                    
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
         MVI   PRSTIM,X'FF'                                                     
         MVI   MRGRTYPE,MRGRHIGH                                                
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
         B     RPX                                                              
*                                                                               
RP10     MVC   PRSTIM,LATEN                                                     
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
         MVC   PRSTIM,LATENX                                                    
         MVI   MRGRTYPE,MRGRHIGH                                                
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         GOTO1 APUTEL,MRGRELEM                                                  
         GOTO1 APUTTAPE                                                         
*                                                                               
RPX      MVC   THISKEY,SAVEKEY                                                  
         XC    TEMP,TEMP                                                        
         B     CNVX                                                             
*                                                                               
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
AMTAB    DS    0XL2                TABLE OF MARKETS THAT DELIVERS 2A-2A         
         DC    AL2(127)                                                         
         DC    AL2(202)                                                         
         DC    AL2(209)                                                         
         DC    AL2(213)                                                         
         DC    AL2(216)                                                         
         DC    AL2(217)                                                         
         DC    AL2(218)                                                         
         DC    AL2(222)                                                         
         DC    AL2(223)                                                         
         DC    AL2(230)                                                         
         DC    AL2(235)                                                         
         DC    AL2(240)                                                         
         DC    AL2(241)                                                         
         DC    AL2(250)                                                         
         DC    AL2(259)                                                         
         DC    AL2(271)                                                         
         DC    AL2(351)                                                         
         DC    AL2(353)                                                         
         DC    AL2(370)                                                         
         DC    AL2(390)                                                         
         DC    X'FF'                                                            
*                                  THIS TIME VALUES                             
THISKEY  DC    XL20'00'                                                         
THISSQH  DC    X'00'                                                            
THISDW   DC    X'00'                                                            
*                                  LAST TIME VALUES                             
PREVKEY  DC    XL20'00'                                                         
PREVSQH  DC    X'00'                                                            
PREVDW   DC    X'00'                                                            
PREVDPT  DS    CL1                                                              
*                                                                               
SAVEKEY  DS    XL20                                                             
MRGEYEC  DC    CL27'*MERGE REPLACEMENT ELEMENT*'                                
*                                                                               
AMFLG    DS    XL1                                                              
*                                                                               
SETACCL  DS    XL1                 SET INTACCS-DISPL. FLAG                      
LATEN    DC    AL1(84)             3A   *MAY BE CHANGED TO 2A*                  
LATENX   DC    AL1(91)             445A                                         
EARLYM   DC    AL1(92)             5A                                           
EARLYMX  DC    AL1(95)             545A                                         
DAY      DC    AL1(0)              6A                                           
DAYX     DC    AL1(83)             245A *MAY BE CHANGED TO 145A*                
*                                                                               
*ATEN    EQU   84                  LATE NIGHT 3AM                               
*ARLYM   EQU   95                  EARLY MORNING 545AM                          
*ARLYD   EQU   00                  EARLY DAY 6AM                                
*ATED    EQU   79                  LATE DAY 245AM                               
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
PAVWRKD  DSECT                                                                  
CONDWORK DS    1000C                                                            
PAVWRKDX EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTOPND                                                      
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DEDAPNO   10/18/13'                                      
         END                                                                    
