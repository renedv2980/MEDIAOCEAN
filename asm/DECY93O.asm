*          DATA SET DECY93O    AT LEVEL 012 AS OF 07/24/08                      
*PHASE DECY93OA                                                                 
         TITLE 'DEMCON - COUNTY COVERAGE OUTPUT PHASE'                          
DECY93O  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 260,**CYDPTO                                                     
         USING DPTWRKD,RC          RC=A(W/S)                                    
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(SORT RECORD)                            
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV4                LAST TIME HOOK                               
*                                                                               
CNV2     CLI   INTRTYP,MLCODEQU    MARKET LIST RECORD                           
         BE    CNV10                                                            
         CLI   INTRTYP,CYCODEQU    COUNTY LISTING RECORD                        
         BE    CNV20                                                            
         CLI   INTRTYP,DRCODEQU    RATINGS RECORD                               
         BE    CNV40                                                            
         B     CNVX                                                             
*                                                                               
CNV4     DS    0H                                                               
         MVI   INTRTYP,X'FF'                                                    
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* BUILD STATION/MARKET, MARKET/STATION & STATION/BOOK RECORDS                   
*                                                                               
CNV10    LA    R6,THISKEY                                                       
         USING BSKEY,R6            BUILD STATION/MARKET RECORD                  
         XC    THISKEY,THISKEY                                                  
         MVI   BSCODE,BSCODEQU     M                                            
         MVI   BSMEDIA,C'U'        U                                            
         MVI   BSSRC,C'N'          N                                            
         MVC   BSBOOK,INTBOOK                                                   
         XC    BSBOOK,=X'FFFF'                                                  
         MVI   BSIND,BSINDEQU      02                                           
         MVC   BSSTAT,INTSTA                                                    
         MVC   BSKMKT,INTCNCD      SPILL MARKET <= COUNTY CODE                  
         MVC   BSBTYP,INTSTCD      BOOKTYPE <= STATE CODE                       
         MVC   BSRMKT,INTDMAC      MARKET <= DMA CODE                           
         GOTO1 ABLDREC,DMCB,(C'P',BSKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING MLKEY,R6            BUILD MARKET/STATION RECORD                  
         XC    THISKEY,THISKEY                                                  
         MVI   MLCODE,MLCODEQU     M                                            
         MVI   MLMEDIA,C'U'        U                                            
         MVI   MLSRC,C'N'          N                                            
         MVC   MLBOOK,INTBOOK                                                   
         XC    MLBOOK,=X'FFFF'                                                  
         MVI   MLIND,MLINDEQU      00                                           
         MVC   MLSTAT,INTSTA                                                    
         MVC   MLRMKT,INTDMAC      MARKET <= DMA CODE                           
         MVC   MLKMKT,INTCNCD      SPILL MARKET <= COUNTY CODE                  
         MVC   MLBTYP,INTSTCD      BOOKTYPE <= STATE CODE                       
         GOTO1 ABLDREC,DMCB,(C'P',MLKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING SBKEY,R6            BUILD STATION/BOOK RECORD                    
         XC    THISKEY,THISKEY                                                  
         MVI   SBCODE,SBCODEQU     S                                            
         MVI   SBMEDIA,C'U'        U                                            
         MVI   SBSRC,C'N'          N                                            
         MVC   SBSTAT,INTSTA                                                    
         MVC   SBKMKT,INTMRKT      SPILL MARKET <= COUNTY CODE                  
         MVC   SBBTYP,INTSTCD      BOOKTYPE <= STATE CODE                       
         MVC   SBRMKT,INTDMAC      MARKET <= DMA CODE                           
         MVC   SBBOOK,INTBOOK                                                   
         GOTO1 ABLDREC,DMCB,(C'P',SBKEY)                                        
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         EJECT                                                                  
* BUILD COUNTY RECORDS                                                          
*                                                                               
CNV20    LA    R6,THISKEY                                                       
         USING CYKEY,R6             BUILD BASIC KEY                             
         XC    THISKEY,THISKEY                                                  
         MVI   CYCODE,CYCODEQU     C                                            
         MVI   CYMEDIA,C'U'        U                                            
         MVI   CYSRC,C'N'          N                                            
         MVC   CYBOOK,INTBOOK                                                   
         XC    CYBOOK,=X'FFFF'                                                  
         MVC   CYSTCD,INTSTCD      STATE CODE                                   
         MVC   CYSTATE,INTSTBV     STATE 2 CHAR ABBREV                          
*                                  BUILD MINOR KEY                              
         MVC   CYCOUNTY,INTCNCD    COUNTY CODE                                  
*                                  BUILD COUNTY NAME ELEMENT                    
         GOTO1 ABLDREC,DMCB,THISKEY                                             
         LA    R6,TEMP                                                          
         USING CYELEM,R6                                                        
         MVI   CYECODE,CYECODEQ                                                 
         MVI   CYLEN,CYLENEQ                                                    
         MVC   CYDMA,INTDMAC       2 BYTE DMA CODE                              
         MVC   CYNAME,INTCNTC      16 CHAR COUNTY NAME                          
         GOTO1 APUTEL,CYELEM                                                    
*                                                                               
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         EJECT                                                                  
* BUILD DEMO RECORDS                                                            
*                                                                               
CNV40    LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
         XC    THISKEY,THISKEY                                                  
         MVI   DRCODE,DRCODEQU                                                  
         MVI   DRMEDIA,C'U'                                                     
         MVI   DRSRC,C'N'                                                       
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRBOOK,INTBOOK                                                   
         XC    DRBOOK,=X'FFFF'                                                  
         MVC   DRKMKT,INTCNCD      SPILL MARKET <= COUNTY CODE                  
         MVC   DRBTYP,INTSTCD      BOOKTYPE <= STATE CODE                       
         LA    R4,INTACCS                                                       
*                                                                               
CNV50    DS    0H                                                               
         MVC   DRHIGHD,INTDAYWK                                                 
         MVC   DRHIQHR,INTDPT      MOVE IN DAYPART CODE TO MINOR KEY            
*                                  BUILD DEMO RECORDS                           
CNV60    GOTO1 ABLDREC,DMCB,THISKEY                                             
         XC    TEMP,TEMP                                                        
         LA    R6,TEMP                                                          
         USING MARELEM,R6          BUILD MARKET ELEMENT                         
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTDMAC                                                    
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
*                                  BUILD DPT MARKET INFO ELEMENT                
         XC    TEMP,TEMP                                                        
         LA    R6,TEMP                                                          
         USING DIELEM,R6                                                        
         MVI   DIECODE,DIECODEQ                                                 
         MVI   DILEN,DIELNEQ                                                    
         MVI   DIDMAIND,C'Y'                                                    
         MVC   DITZ,INTZONE                                                     
         MVC   DIAFFL,INTAFFC                                                   
         MVC   DICHNL,INTCHAN                                                   
         GOTO1 APUTEL,DIELEM                                                    
*                                                                               
         XC    TEMP,TEMP           COUNTY NAME ELEMENT                          
         LA    R6,TEMP                                                          
         USING CNTYELD,R6                                                       
         MVI   CNTYEL,CNTYELQ                                                   
         MVI   CNTYELEN,CNTYLENQ                                                
         MVC   CNTYCODE,INTCNCD                                                 
         MVC   CNTYNAME,INTCNTC                                                 
         GOTO1 APUTEL,CNTYELD                                                   
*                                                                               
         XC    TEMP,TEMP           DMA NAME ELEMENT                             
         LA    R6,TEMP                                                          
         USING DMAELD,R6                                                        
         MVI   DMAEL,DMAELQ                                                     
         MVI   DMAELLEN,DMALENQ                                                 
         MVC   DMACODE,INTDMAC                                                  
         MVC   DMANAME,INTDMA                                                   
         MVC   DMAORIC,INTORIC     MKT OF ORIGIN CODE                           
         GOTO1 APUTEL,DMAELD                                                    
*                                                                               
         XC    TEMP,TEMP                                                        
         LA    R6,TEMP                                                          
         USING QHELEM,R6                                                        
         MVI   QHCODE,QHCODEQ                                                   
         MVI   QHELN,QHPNAME-QHCODE+14                                          
         MVC   QHSQH,INTSQH                                                     
         MVC   QHEQH,INTEQH                                                     
         MVC   QHDAY,INTDAYWK                                                   
         MVC   QHPNAME(14),INTORIG                                              
         GOTO1 APUTEL,QHELEM                                                    
*                                                                               
         XC    TEMP,TEMP                                                        
         LA    R6,TEMP                                                          
         USING SLELEM,R6                                                        
         MVI   SLCODE,SLCODEQ                                                   
         MVI   SLLEN,3                                                          
*        MVC   SLSECT,INTDPT                                                    
         MVC   SLSECT,INTSQH                                                    
         GOTO1 APUTEL,SLELEM                                                    
*                                                                               
         LA    R7,DBLOCKA                                                       
         USING DBLOCKD,R7                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'CUN'                                                   
         MVI   DBSELMED,C'U'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELBK,INTBOOK                                                  
         ST    RA,DBCOMFCS                                                      
         MOVE  (CONDLEN,1000),INTACCS                                           
         GOTO1 CDEMEL,DMCB,(C'C',0),DBLOCKD,CONDLEN                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R1,CONDWRK1                                                      
CNV70    CLI   0(R1),0                                                          
         BE    CNV80                                                            
         GOTO1 APUTEL,(R1)                                                      
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     CNV70                                                            
*                                                                               
CNV80    GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
*                                                                               
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                  THIS TIME/LAST TIME VALUES                   
THISKEY  DC    XL20'00'                                                         
PREVKEY  DC    XL20'00'                                                         
*                                                                               
MAXLEN   DC    H'1000'             MAX DEMO RECORD SIZE                         
         SPACE 2                                                                
* DSECT TO COVER TEMPORARY W/S                                                  
*                                                                               
DPTWRKD  DSECT                                                                  
AREC     DS    A                                                                
AFRSTEL  DS    A                                                                
RECLEN   DS    H                                                                
CONDFLAG DS    X                                                                
CONDLEN  DS    XL2                                                              
CONDWRK1 DS    1000C                                                            
CONDWRK2 DS    1000C                                                            
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTCYD                                                       
         EJECT                                                                  
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DECOUNTYD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DECOUNTYD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012DECY93O   07/24/08'                                      
         END                                                                    
