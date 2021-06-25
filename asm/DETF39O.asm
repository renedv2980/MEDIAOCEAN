*          DATA SET DETF39O    AT LEVEL 006 AS OF 12/02/11                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETF39OA                                                                 
         TITLE 'DEMCON - FUSION CONVERSION - OUTPUT PHASE'                      
DETF39O  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TPTWRKX-TPTWRKD,DETF39O                                          
         USING TPTWRKD,RC          RC=A(W/S)                                    
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(SORT RECORD)                            
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV4                LAST TIME HOOK                               
*                                                                               
CNV2     CLI   INTRTYP,BSCODEQU    STATION/MARKET RECORD                        
         BE    CNV10                                                            
         CLI   INTRTYP,DRCODEQU    RATINGS RECORD                               
         BE    CNV14                                                            
         B     CNVX                                                             
*                                                                               
CNV4     MVI   INTRTYP,X'FF'                                                    
         B     CNV16               GO WRITE LAST RECORD                         
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* BUILD STATION/MARKET, MARKET/STATION & STATION/BOOK RECORDS                   
*                                                                               
CNV10    LA    R6,THISKEY                                                       
         USING BSKEY,R6            BUILD STATION/MARKET RECORD                  
         XC    THISKEY,THISKEY                                                  
         MVI   BSCODE,BSCODEQU                                                  
         MVC   BSMEDIA,MEDIA                                                    
         MVC   BSSRC,OUTSRC                                                     
         MVC   BSBOOK,INTBOOK                                                   
         XC    BSBOOK,=X'FFFF'                                                  
         MVI   BSIND,BSINDEQU                                                   
         MVC   BSSTAT,INTSTA                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   BSKMKT,INTMRKT                                                   
         MVC   BSSTYP,INTSTYP                                                   
         MVC   BSBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   BSRMKT,INTMRKT                                                   
         GOTO1 ABLDREC,DMCB,(C'P',BSKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING MLKEY,R6            BUILD MARKET/STATION RECORD                  
         XC    THISKEY,THISKEY                                                  
         MVI   MLCODE,MLCODEQU                                                  
         MVC   MLMEDIA,MEDIA                                                    
         MVC   MLSRC,OUTSRC                                                     
         MVC   MLBOOK,INTBOOK                                                   
         XC    MLBOOK,=X'FFFF'                                                  
         MVI   MLIND,MLINDEQU                                                   
         MVC   MLRMKT,INTMRKT                                                   
         MVC   MLSTAT,INTSTA                                                    
         XC    MLKMKT,MLKMKT                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   MLKMKT,INTMRKT                                                   
         MVC   MLBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         GOTO1 ABLDREC,DMCB,(C'P',MLKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING SBKEY,R6            BUILD STATION/BOOK RECORD                    
         XC    THISKEY,THISKEY                                                  
         MVI   SBCODE,SBCODEQU                                                  
         MVC   SBMEDIA,MEDIA                                                    
         MVC   SBSRC,OUTSRC                                                     
         MVC   SBSTAT,INTSTA                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   SBKMKT,INTMRKT                                                   
         MVC   SBSTYP,INTSTYP                                                   
         MVC   SBBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   SBRMKT,INTMRKT                                                   
         MVC   SBBOOK,INTBOOK                                                   
         GOTO1 ABLDREC,DMCB,(C'P',SBKEY)                                        
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         EJECT                                                                  
* BUILD DEMO RECORDS                                                            
*                                                                               
CNV14    LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
         XC    THISKEY,THISKEY                                                  
         MVI   DRCODE,DRCODEQU                                                  
         MVC   DRMEDIA,MEDIA                                                    
         MVC   DRSRC,OUTSRC                                                     
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRBOOK,INTBOOK                                                   
         XC    DRBOOK,=X'FFFF'                                                  
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   DRKMKT,INTMRKT                                                   
         MVC   DRSTYP,INTSTYP                                                   
         MVC   DRBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVI   CONDFLAG,0          SET DEMO ELEMENT BUILD FLAG                  
         CLC   THISKEY(L'DRKMAJOR),PREVKEY                                      
         BE    CNV28                                                            
*                                  MAJOR KEY CONTROL BREAK                      
CNV16    CLI   PUTSW,C'Y'          TEST IF LAST RECORD WRITTEN                  
         BE    CNV18                                                            
         L     R6,AOREC                                                         
         LA    R6,4(R6)                                                         
         MVC   DRHIGHD,PREVDAY     SET HIGH DAY & QTR HOUR IN KEY               
         MVC   DRHIQHR,PREVQHR                                                  
         GOTO1 APUTTAPE            PUT LAST RECORD TO TAPE                      
         MVI   PUTSW,C'Y'                                                       
*                                                                               
CNV18    XC    PREVKEY,PREVKEY     CLEAR LAST TIME VALUES                       
         XC    PREVPNAM,PREVPNAM                                                
         CLI   INTRTYP,X'FF'       EXIT IF LAST TIME HOOK                       
         BNE   CNV20                                                            
         MVI   INTRTYP,DRCODEQU                                                 
         B     CNVX                                                             
*                                  BUILD DEMO RECORDS                           
CNV20    GOTO1 ABLDREC,DMCB,THISKEY                                             
         MVC   PREVKEY,THISKEY                                                  
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING MARELEM,R6          BUILD MARKET ELEMENT                         
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT                                                    
         MVC   MARSTYP,INTSTYP                                                  
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING NCCELEM,R6          BUILD NCC STATION CALL ELEMENT               
         MVI   NCCCODE,NCCCODEQ                                                 
         MVI   NCCLEN,NCCLENEQ                                                  
         MVC   NCCSCALL,INTSCALL                                                
         GOTO1 APUTEL,NCCELEM                                                   
         DROP  R6                                                               
*                                                                               
*                                  BUILD QTR HOUR ELEMENT                       
CNV28    LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING QHELEM,R6                                                        
         MVI   QHCODE,QHCODEQ                                                   
         MVC   QHDAY,INTDAYWK                                                   
         MVC   QHSQH,INTSQH                                                     
         MVC   QHEQH,INTEQH                                                     
*                                                                               
         MVC   QHWKS,INTSWKS       BITS REPRESENTING WEEK NUMBERS               
         CLI   INTS#WKS,1          1-WEEK PROGRAM?                              
         BE    CNV29               YES: NEVER PREDOMINANT PROGRAM               
         CLI   INTS#WKS,3          3- OR 4-WEEK PROGRAM?                        
         BL    *+12                NO, IT'S A 2-WEEK PROGRAM                    
         OI    QHWKS,X'20'         YES: ALWAYS SET PREDOMINANT FLAG             
         B     CNV29                                                            
*                                                                               
         CLC   PREVDAY,QHDAY       FIRST RECORD FOR THIS QUARTER-HOUR?          
         BNE   CNV29                                                            
         CLC   PREVQHR,QHSQH                                                    
         BNE   CNV29                                                            
*                                  NO: THIS IS EITHER THE PREDOMINANT           
*                                  PROGRAM OF A 2-2 SPLIT, OR IT'S THE          
*                                  2-WEEK PROGRAM IN A 2-1-1 SPLIT              
         OI    QHWKS,X'20'         SET PREDOMINANT FLAG                         
         CLI   PREV#WKS,2          IS THIS A 2-2 SPLIT?                         
         BNE   CNV29               NO: IT'S A 2-1-1 SPLIT                       
         OI    QHWKS,X'10'         YES: SET 2-WK AVERAGE FLAG                   
*                                                                               
CNV29    DS    0H                                                               
         MVC   QHPNAME(L'INTPNAM),INTPNAM                                       
         LA    R1,QHPNAME+L'INTPNAM-1                                           
         CLI   0(R1),C' '          LOCATE END OF PROGRAM NAME                   
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         SR    R1,R6                                                            
         STC   R1,QHELN            SET ELEMENT LENGTH                           
*                                                                               
         CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
         BNE   CNV30                                                            
         GOTO1 APUTEL,QHELEM       NO - ADD ELEMENT                             
*                                                                               
CNV30    CLI   CONDFLAG,0          TEST IF DEMO ELEMENTS BUILT                  
         BNE   CNV33                                                            
*                                                                               
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
*                                                                               
         CLI   OUTSRC,C'A'         FORCE TO NSI IF ARB                          
         BNE   *+8                 SAME FORMAT                                  
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         MVC   DBSELBK,INTBOOK                                                  
         ST    RA,DBCOMFCS                                                      
         MOVE  (CONDLEN,1000),INTACCS                                           
         GOTO1 CDEMEL,DMCB,(C'C',0),DBLOCKD,CONDLEN                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CONDFLAG,1                                                       
         DROP  R5                                                               
*                                                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'CORETAB'),CONDLEN,TEMP2                     
*                                                                               
CNV33    CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
         BNE   CNV38                                                            
         LA    R1,CONDWRK1         NO - ADD DEMO ELEMENTS TO RECORD             
         SR    R0,R0                                                            
*                                                                               
CNV34    CLI   0(R1),0             TEST E-O-L                                   
         BE    CNV36                                                            
         GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CNV34                                                            
*                                  SAVE THIS TIME VALUES                        
CNV36    MVC   PREVDAY,INTDAYWK                                                 
         MVC   PREVQHR,INTEQH                                                   
         MVC   PREVPNAM,INTPNAM                                                 
         MVC   PREV#WKS,INTS#WKS                                                
         MVI   PUTSW,C'N'          SET DEMO RECORD NOT WRITTEN                  
         B     CNVX                                                             
         EJECT                                                                  
* CONDENSE QTR HOUR & DEMO ELEMENTS. PROGRAM NAME WILL BE DROPPED FROM          
* QTR HR ELEMENT IF IT MATCHES PREVIOUS. 4BYTE DEMO ELEMENTS WILL BE            
* CREATED IF ELEMENTS EXACTLY MATCH ANY PREVIOUS QTR HR ON THIS RECORD.         
*                                                                               
CNV38    LA    R1,CONDWRK2         R1=A(OUTPUT,STRING)                          
         LA    R6,TEMP                                                          
         USING QHELEM,R6           R6=A(QTR HOUR ELEMENT)                       
         LLC   RE,QHELN            GET LENGTH (INCLUDING PROGRAM NAME)          
         CLC   INTPNAM,PREVPNAM    TEST IF PROGRAM NAME MATCHES PREV.           
         BNE   *+8                                                              
         LA    RE,QHPNAME-QHELEM   GET LENGTH (EXCLUDING PROGRAM NAME)          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),QHELEM      MOVE ELEMENT TO OUTPUT STRING                
         STC   RE,1(R1)            SET NEW ELEMENT LENGTH                       
         AR    R1,RE                                                            
         MVI   0(R1),0                                                          
         L     R6,AOREC                                                         
         LA    R6,4(R6)                                                         
         USING DRKEY,R6                                                         
         ST    R6,AREC                                                          
         MVC   RECLEN,DRRLEN       SAVE CURRENT OUTPUT RECORD LENGTH            
         LA    R6,DRFRSTEL                                                      
         ST    R6,AFRSTEL          SAVE A(FIRST ELEMENT)                        
         LA    R2,CONDWRK1         R2=A(INPUT STRING)                           
*                                  CONDENSE DEMO ELEMENTS                       
CNV40    CLI   0(R2),0             TEST E-O-L                                   
         BE    CNV48                                                            
         L     R6,AFRSTEL          R6=A(FIRST ELEMENT)                          
*                                  FIND MATCH WITH PREVIOUS ELEMENT             
CNV42    CLI   0(R6),0             TEST E-O-R                                   
         BE    CNV44                                                            
         IC    RE,1(R6)                                                         
         BCTR  RE,0                GET L'ELEMENT-1                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),0(R2)       MATCH RECORD EL WITH INPUT EL                
         BE    *+12                                                             
         LA    R6,1(RE,R6)         NOT EQUAL - BUMP TO NEXT                     
         B     CNV42                                                            
*                                  BUILD DUPLICATE DEMO ELEMENT                 
         MVC   0(1,R1),0(R2)                                                    
         MVI   1(R1),4                                                          
         S     R6,AREC                                                          
         STCM  R6,3,2(R1)          SET DISP INTO RECORD OF DUPLICATE            
         OI    2(R1),X'80'                                                      
         LA    R1,4(R1)                                                         
         B     CNV46                                                            
*                                  MOVE INPUT EL TO OUTPUT STRING               
CNV44    IC    RE,1(R2)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)                                                    
         AR    R1,RE                                                            
*                                  BUMP TO NEXT INPUT ELEMENT                   
CNV46    MVI   0(R1),0                                                          
         IC    RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     CNV40                                                            
*                                  TEST IF CONDENSED STRING WILL FIT            
CNV48    LA    RE,CONDWRK2                                                      
         SR    R1,RE                                                            
         AH    R1,RECLEN                                                        
         CH    R1,MAXLEN                                                        
         BNH   CNV50                                                            
*                                  NO - WRITE PREVIOUS DEMO RECORD              
         MVI   PUTSW,C'N'                                                       
         B     CNV16                                                            
*                                  YES - ADD CONDENSED ELS TO RECORD            
CNV50    LA    R1,CONDWRK2                                                      
         SR    R0,R0                                                            
         B     CNV34                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
TEMP2    EQU   TEMP+50                                                          
THISKEY  DC    XL20'00'                                                         
PREVKEY  DC    XL20'00'                                                         
PREVDAY  DC    X'00'                                                            
PREVQHR  DC    X'00'                                                            
PREVPNAM DC    XL14'00'                                                         
PREV#WKS DC    X'00'               WEEK FREQUENCY IN PREVIOUS RECORD            
*                                                                               
PUTSW    DC    C'Y'                Y=PREVIOUS RECORD WRITTEN                    
MAXLEN   DC    H'1000'             MAX DEMO RECORD SIZE                         
         EJECT                                                                  
* DSECT TO COVER TEMPORARY W/S                                                  
*                                                                               
TPTWRKD  DSECT                                                                  
AREC     DS    A                                                                
AFRSTEL  DS    A                                                                
RECLEN   DS    H                                                                
*                                                                               
CONDFLAG DS    X                                                                
CONDLEN  DS    XL2                                                              
CONDWRK1 DS    1000C                                                            
CONDWRK2 DS    1000C                                                            
*                                                                               
TPTWRKX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTFUSD                                                      
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
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DETF39O   12/02/11'                                      
         END                                                                    
