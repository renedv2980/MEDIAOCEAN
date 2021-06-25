*          DATA SET DERC09O    AT LEVEL 001 AS OF 06/12/09                      
*PHASE DERC09OA                                                                 
*---------------------------------------------------------------------*         
* RADIO COUNTY COVERAGE CONVERSION: OUTPUT PHASE.                     *         
*                                                                     *         
* IPHASE: DERC09I                                                     *         
* OPHASE: DERC09O                                                     *         
*---------------------------------------------------------------------*         
         TITLE '- DEMO CONVERSION - RADIO COUNTY COVERAGE'                      
DERC09O  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DERC09O,RA                                           
         USING WORKD,RC            RC=A(TEMP W/S)                               
         USING DPRINT,R7           R7=PRINT                                     
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
*                                                                               
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD AFTER SORT)              
*                                                                               
         B     *+4(R1)                                                          
         B     CNV00               PROCESS A RECORD                             
         B     LASTHK              LAST TIME HOOK                               
*                                                                               
* PROCESS A RECORD                                                              
CNV00    MVI   PRINTSW,0                                                        
*                                                                               
         CLI   FRSTREC,YES         FIRST RECORD TO OPHASE                       
         BNE   CNV25                                                            
         MVC   P,SPACES                                                         
         GOTO1 VDEPRNT2                                                         
         MVC   P(24),=C'** STARTING OUTPUT PHASE'                               
         GOTO1 VDEPRNT2                                                         
CNV25    MVI   FRSTREC,NO                                                       
*                                                                               
CNV30    CLI   INTRTYP,DRCODEQU    MAIN RECORD                                  
         BNE   *+12                                                             
         BAS   RE,MAINCNV                                                       
         B     CNVX                                                             
*                                                                               
         CLI   INTRTYP,SBCODEQU    PASSIVE RECORD                               
         BNE   *+12                                                             
         BAS   RE,PASSCNV                                                       
         B     CNVX                                                             
*                                                                               
         CLI   INTRTYP,CYCODEQU    COUNTY RECORD                                
         BNE   *+12                                                             
         BAS   RE,CNTYCNV                                                       
         B     CNVX                                                             
*                                                                               
         DC    H'0'                INVALID RECORD TYPE                          
*                                                                               
* LAST TIME HOOK                                                                
LASTHK   DS    0H                                                               
LASTHKX  B     CNVX                                                             
*                                                                               
CNVX     XMOD1                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS MAIN RECORDS                                                          
***********************************************************************         
MAINCNV  NTR1                                                                   
*                                                                               
         XC    THISKEY,THISKEY     BUILD KEY OF THE MAIN RECORD                 
         LA    RE,THISKEY                                                       
         USING DRKEY,RE                                                         
         MVI   DRCODE,DRCODEQU                                                  
         MVI   DRMEDIA,C'U'                                                     
         MVI   DRSRC,C'A'                                                       
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRBOOK,INTBOOK                                                   
         XC    DRBOOK,=X'FFFF'                                                  
         MVC   DRKMKT,INTCNTY                                                   
         MVC   DRBTYP,INTSTATE                                                  
         MVC   DRHOME,INTTIER                                                   
         MVC   DRHIGHD,INTDPT                                                   
         DROP  RE                                                               
         GOTO1 ABLDREC,DMCB,THISKEY                                             
*                                                                               
         XC    TEMP,TEMP           BUILD MARKET ELEMENT '01'                    
         LA    R6,TEMP                                                          
         USING MARELEM,R6                                                       
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTCNTY       COUNTY CODE                                  
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP             BUILD THE RADIO CNTY COV ELEM '07'           
         XC    TEMP,TEMP                                                        
         USING CYRELEM,R6                                                       
         MVI   CYRCODE,CYRCODEQ    ELEMENT CODE                                 
         MVC   CYRLSTAT,INTSTSTA   STATE OF LICENSE                             
         MVC   CYRLCITY,INTSTCIT   CITY OF LICENSE                              
         MVC   CYRLCNTY,INTSTCTY   COUNTY OF LICENSE                            
         MVC   CYRDQHDR,INTDPQH    DAYPART TOTAL QH DURATION                    
         MVC   CYRDMILS,INTDMILS   DAYPART MILITARY START TIME                  
         MVC   CYRDMILE,INTDMILE   DAYPART MILITARY END TIME                    
         MVC   CYRAFFL,INTSTAFS    AFFILIATIONS                                 
         LA    RE,CYRAFFL+L'CYRAFFL-1   LAST BYTE OF AFFILATIONS FIELD          
         LA    RF,CYRAFFL                                                       
MAINC20  CR    RE,RF               FIND LAST NON-BLANK CHARACTER                
         BL    MAINC25             MAKE SURE WE DIDN'T REACH PREV FIELD         
         CLI   0(RE),C' '                                                       
         BNE   MAINC25             FOUND THE LAST NON-BLANK CHARACTER           
         SHI   RE,1                                                             
         B     MAINC20                                                          
MAINC25  LA    RF,CYRELEM          COMPUTE LENGTH FROM START OF ELEMENT         
         SR    RE,RF                                                            
         AHI   RE,1                                                             
*                                                                               
         CLC   CYRLSTAT(L'CYRLSTAT+L'CYRLCITY+L'CYRLCNTY),SPACES                
         BNE   *+8                 DON'T INCLUDE BLANK STATION INFO             
         SHI   RE,L'CYRLSTAT+L'CYRLCITY+L'CYRLCNTY                              
*                                                                               
         STC   RE,CYRELLN                                                       
         GOTO1 APUTEL,CYRELEM                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP             BUILD THE QUARTER HOUR ELEMENT '20'          
         XC    TEMP,TEMP                                                        
         USING QHELEM,R6                                                        
         MVI   QHELN,QHPNAME-QHCODE+14                                          
         MVI   QHCODE,QHCODEQ      ELEMENT CODE                                 
         MVC   QHDAY,INTDAYWK      DAY CODE                                     
         MVC   QHSQH,INTSQH        START QUARTER HOUR                           
         MVC   QHEQH,INTEQH        END QUARTER HOUR                             
         MVC   QHPNAME(14),INTDPTNM DAYPART NAME                                
         GOTO1 APUTEL,QHELEM                                                    
         DROP  R6                                                               
*                                                                               
         LA    R5,DBLOCKA           BUILD DEMO ELEMENTS                         
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'RUA'                                                   
         MVI   DBSELMED,C'U'                                                    
         MVI   DBSELSRC,C'A'                                                    
         MVC   DBSELBK,INTBOOK                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         LHI   R1,CONWRKLN                                                      
         MOVE  (CONDWORK,(R1)),INTACCS                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         ICM   RF,15,CDEMEL                                                     
         DROP  RF                                                               
         GOTO1 (RF),DMCB,(C'C',0),DBLOCKD,CONDWORK                              
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
                                                                                
         OC    CONDWORK(2),CONDWORK   TEST FOR ANY DEMO ELEMENTS                
         BZ    MAINC55                                                          
         LA    R1,CONDWORK+2       START OF DEMO ELEMENTS                       
*                                                                               
MAINC50  CLI   0(R1),0             ADD DEMO ELEMENTS TO RECORD                  
         BE    MAINC55                                                          
         GOTO1 APUTEL,(R1)                                                      
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     MAINC50                                                          
MAINC55  DS    0X                                                               
*                                                                               
         GOTO1 APUTTAPE                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS PASSIVE RECORDS                                                       
***********************************************************************         
PASSCNV  NTR1                                                                   
*                                                                               
         XC    THISKEY,THISKEY     BUILD THE STATION/BOOK LIST RECORDS          
         LA    RE,THISKEY                                                       
         USING SBKEY,RE                                                         
         MVI   SBCODE,SBCODEQU                                                  
         MVI   SBMEDIA,C'U'                                                     
         MVI   SBSRC,C'A'                                                       
         MVC   SBSTAT,INTSTA                                                    
         MVC   SBKMKT,INTCNTY                                                   
         MVC   SBBTYP,INTSTATE                                                  
         MVC   SBRMKT,INTCNTY                                                   
         MVC   SBBOOK,INTBOOK                                                   
         MVC   SBHOME,INTTIER                                                   
         DROP  RE                                                               
         GOTO1 ABLDREC,DMCB,(C'P',THISKEY)                                      
         GOTO1 APUTTAPE                                                         
*                                                                               
         XC    THISKEY,THISKEY     BUILD THE MARKET LIST RECORDS                
         LA    RE,THISKEY                                                       
         USING MLKEY,RE                                                         
         MVI   MLCODE,MLCODEQU                                                  
         MVI   MLMEDIA,C'U'                                                     
         MVI   MLSRC,C'A'                                                       
         MVC   MLBOOK,INTBOOK                                                   
         XC    MLBOOK,=X'FFFF'                                                  
         MVC   MLRMKT,INTCNTY                                                   
         MVC   MLSTAT,INTSTA                                                    
         MVC   MLKMKT,INTCNTY                                                   
         MVC   MLBTYP,INTSTATE                                                  
         MVC   MLHOME,INTTIER                                                   
         DROP  RE                                                               
         GOTO1 ABLDREC,DMCB,(C'P',THISKEY)                                      
         GOTO1 APUTTAPE                                                         
*                                                                               
         XC    THISKEY,THISKEY     BUILD THE STATION LIST RECORDS               
         LA    RE,THISKEY                                                       
         USING BSKEY,RE                                                         
         MVI   BSCODE,BSCODEQU                                                  
         MVI   BSMEDIA,C'U'                                                     
         MVI   BSSRC,C'A'                                                       
         MVC   BSBOOK,INTBOOK                                                   
         XC    BSBOOK,=X'FFFF'                                                  
         MVI   BSIND,BSINDEQU                                                   
         MVC   BSSTAT,INTSTA                                                    
         MVC   BSKMKT,INTCNTY                                                   
         MVC   BSBTYP,INTSTATE                                                  
         MVC   BSRMKT,INTCNTY                                                   
         MVC   BSHOME,INTTIER                                                   
         DROP  RE                                                               
         GOTO1 ABLDREC,DMCB,(C'P',THISKEY)                                      
         GOTO1 APUTTAPE                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS COUNTY RECORDS                                                        
***********************************************************************         
CNTYCNV  NTR1                                                                   
*                                                                               
         XC    THISKEY,THISKEY     BUILD KEY OF THE COUNTY RECORD               
         LA    RE,THISKEY                                                       
         USING CYKEY,RE                                                         
         MVI   CYCODE,CYCODEQU                                                  
         MVI   CYMEDIA,C'U'                                                     
         MVI   CYSRC,C'A'                                                       
         MVC   CYBOOK,INTBOOK                                                   
         XC    CYBOOK,=X'FFFF'                                                  
         MVC   CYSTCD,INTSTATE                                                  
         MVC   CYCOUNTY,INTCNTY                                                 
         DROP  RE                                                               
         GOTO1 ABLDREC,DMCB,THISKEY                                             
*                                                                               
         XC    TEMP,TEMP           BUILD COUNTY NAME ELEMENT                    
         LA    R6,TEMP                                                          
         USING DMELEM,R6                                                        
         MVI   DMECODE,DMECODEQ                                                 
         MVC   DMMNO+1(1),INTTIER                                               
         MVC   DMMNAME(L'INTCTYNA),INTCTYNA                                     
         LA    RE,DMMNAME+L'INTCTYNA-1   LAST BYTE OF COUNTY NAME               
         LA    RF,DMMNAME                                                       
CNTYC20  CR    RE,RF               FIND LAST NON-BLANK CHARACTER                
         BL    CNTYC25             MAKE SURE WE DIDN'T REACH PREV FIELD         
         CLI   0(RE),C' '                                                       
         BNE   CNTYC25             FOUND THE LAST NON-BLANK CHARACTER           
         SHI   RE,1                                                             
         B     CNTYC20                                                          
CNTYC25  LA    RF,DMELEM           COMPUTE LENGTH FROM START OF ELEMENT         
         SR    RE,RF                                                            
         AHI   RE,1                                                             
         STC   RE,DMLEN                                                         
         GOTO1 APUTEL,DMELEM                                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 APUTTAPE                                                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------            
* LTORG, TABLES, AND CONSTANTS                                                  
*-------------------------------------------------------------------            
         LTORG                                                                  
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
FRSTREC  DC    AL1(YES)            FIRST RECORD TO OPHASE                       
*                                                                               
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER TEMP W/S                                                       
**********************************************************************          
*                                                                               
WORKD    DSECT                                                                  
THISKEY  DS    XL23                                                             
*                                                                               
CONDWORK DS    (CONWRKLN)X                                                      
CONWRKLN EQU   1000                                                             
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
*                                                                               
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
* INTERIM RECORD DSECT                                                          
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTRCYD                                                      
*                                                                               
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DERC09O   06/12/09'                                      
         END                                                                    
