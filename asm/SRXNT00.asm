*          DATA SET SRXNT00    AT LEVEL 011 AS OF 03/26/19                      
*PHASE T12200A                                                                  
         TITLE '$XTNT - DISPLAY FILE EXTENTS'                                   
         PRINT NOGEN                                                            
EXTENT   RSECT                                                                  
         NMOD1 WORKL,*$XTNT**,RA,R9,RR=R2,CLEAR=YES                             
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         ST    R2,RELO                                                          
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         MVC   ATIOB,SRQATIOB                                                   
         MVC   ATIA,SRQATIA                                                     
         MVC   ASYSFACS,SRPARM1                                                 
*                                                                               
         L     R3,SRPARM6                                                       
         USING SRXNTFFD,R3         R3=A(TWA)                                    
*                                                                               
         BRAS  RE,INIT                                                          
         BRAS  RE,P1VAL                                                         
         BRAS  RE,P2VAL                                                         
         BRAS  RE,P3VAL                                                         
         BRAS  RE,P4VAL                                                         
*                                                                               
         OC    PRTUSER,PRTUSER     OUTPUT TO PRTQUE                             
         BZ    *+12                                                             
         BRAS  RE,PRTFST           INITIALISE REPORT                            
         B     MAIN02                                                           
*                                                                               
         BRAS  RE,SYSDEF           SET DEFAULTS FOR SINGLE SYSTEM               
*                                                                               
         CLI   FIL,0               SINGLE FILE DISPLAY                          
         BE    *+10                                                             
         BRAS  RE,FILDSP                                                        
         DC    H'0'                SHOULD NEVER RETURN                          
*                                                                               
MAIN02   BRAS  RE,SYSDSP                                                        
         DC    H'0'                SHOULD NEVER RETURN                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE P1                                                         *         
***********************************************************************         
P1VAL    NTR1                                                                   
         LA    R4,SRVP1H           P1=SYSTEM(,FILE,XTNT)                        
         USING FHD,R4                                                           
         ST    R4,CURSOR                                                        
         MVC   SYSINDEX,=F'1'                                                   
         CLI   FHIL,0                                                           
         BNE   *+14                                                             
         MVI   FHIL,3                                                           
         MVC   FHDA(3),=CL3'ALL'                                                
*                                                                               
         LA    R8,XNTINFO                                                       
         USING SCANBLKD,R8                                                      
         GOTO1 ASCANNER,PARM,(R4),(X'86',(R8))                                  
*                                                                               
         MVC   FERRDSP,SC1STNUM                                                 
         CLI   4(R1),0                                                          
         BE    EMISIF                                                           
         CLI   4(R1),3                                                          
         BH    ETMIF                                                            
*                                                                               
         CLI   SC2NDLEN,0          DEFAULT IS ALL SYSTEMS (SYS=0)               
         BNE   EIIFF                                                            
         CLI   SC1STLEN,7          1-7 CHARACTERS                               
         BH    ETOOL                                                            
*                                                                               
P1V00    CLI   SC1STLEN,0          DEFAULT IS "ALL"                             
         BE    P1V00A                                                           
         MVC   SYSNAME,SC1STFLD                                                 
         CLC   SYSNAME(4),=C'ALL ' ALL EXPLICITLY SET                           
         BE    P1V00A                                                           
         CLC   SYSNAME(4),=C'ALL,'                                              
         BNE   P1V01                                                            
P1V00A   CLI   RW,C' '             SET DEFAULT FOR ALL SYSTEMS TO +             
         BNE   P1V02                                                            
         MVI   RW,C'+'                                                          
         B     P1V02                                                            
*                                                                               
P1V01    L     R5,ASYSFACS                                                      
         L     R5,VSELIST-SYSFACD(R5) SEARCH SELIST FOR SYSTEM                  
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         CLC   SENAME,SYSNAME                                                   
         BE    *+12                                                             
         BXLE  R5,RE,*-10                                                       
         B     EINSYS                                                           
*                                                                               
         TM    SEIND,SEINOP        SYSTEM MUST BE OPEN                          
         BO    ESYSNO                                                           
         TM    SEIND,SEISTRT       AND STARTED                                  
         BZ    ESYSNO              SYSTEM NOT OPERATIONAL                       
*                                                                               
         OC    SEFILES,SEFILES     SYSTEM MUST HAVE FILES                       
         BZ    ESYSNO                                                           
         MVC   SYS,SESYS           SAVE SYSTEM NUMBER                           
*                                                                               
         XC    DMCB(24),DMCB       SET UP DMCB FOR LOCKSPC                      
         MVC   DMCB+3(1),SYS                                                    
         GOTO1 ALOCKSPC,DMCB       LOCK THIS SYSTEM                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+1,5                                                         
         MVC   DMCB+3(1),SYS                                                    
         GOTO1 ALOCKSPC,DMCB       REFRESH SYSTEM CALL                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'10'                                                       
         MVC   DMCB+3(1),SYS                                                    
         GOTO1 ALOCKSPC,DMCB       UNLOCK THE SYSTEM                            
*                                                                               
P1V02    XR    R0,R0               FIND START OF SYSTEM FILES                   
         ICM   R0,1,SYS                                                         
         BNZ   *+8                                                              
         LHI   R0,1                                                             
         GOTO1 ADATAMGR,DMCB,DMREAD,SYSFLES,(R0)                                
         MVC   SYSADDR+1(3),13(R1)                                              
*                                                                               
         CLI   FHIL,0              DEFAULT IS ALL FILES FOR SYSTEM              
         BE    P1VALX                                                           
         CLI   PARM+4,2            JUST ONE PARAMETER INPUT?                    
         BL    P1VALX              YES                                          
*                                                                               
         MVI   STINDFLG,C'Y'       REMEMBER THAT A START WAS PROVIDED           
         AHI   R8,SCBLKLQ          NEXT FIELD IS START INDEX                    
         MVC   FERRDSP,SC1STNUM                                                 
         CLI   SC2NDLEN,0                                                       
         BNE   EIIFF               NO SECOND PART TO ANY OF THE FIELDS          
*                                                                               
         TM    SC1STVAL,SCNUMQ     IF INTEGER ITS DISPLAY START INDEX           
         BZ    P1V10                                                            
         XR    RF,RF                                                            
         ICM   RF,7,SC1STNUM+1                                                  
         BZ    EIIFF                                                            
         STCM  RF,15,SYSINDEX                                                   
         L     R5,SYSADDR          Get start of system list                     
         USING SYSFLSTD,R5                                                      
         LA    R5,SYSFLIST         Get to actual list of files                  
         SHI   RF,1                R0=index number to file we want              
         MHI   RF,SYSFLNQ                                                       
         AR    R5,RF               A(FILE) we want                              
         MVC   FIL,SYSFILE#                                                     
         ST    R5,FILADDR                                                       
         B     P1V16                                                            
*                                                                               
P1V10    CLI   SC1STLEN,7          IF ALPHA VALUE IS THE FILENAME               
         BH    ETOOL                                                            
         MVC   FILNAME,SC1STFLD                                                 
         CLI   SYS,0                                                            
         BE    EIFID                                                            
*                                                                               
         L     R5,SYSADDR          SEARCH FOR FILE NAME IN SYSTEM               
         USING SYSFLSTD,R5                                                      
         LLH   R0,SYSF#FLS         R0=NUM OF FILES                              
         LR    RE,R0               SAVE NUMBER OF FILES                         
         LA    R5,SYSFLIST                                                      
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
                                                                                
P1V12    L     R6,SYSFADTF-1       R7=A(DTF)                                    
         USING ISDTF,R6                                                         
         EX    RF,*+8              MATCH FILENAME                               
         B     *+10                                                             
         CLC   FILNAME(0),ISFDD                                                 
         BE    P1V14                                                            
         AHI   R5,SYSFLNQ                                                       
         BCT   R0,P1V12                                                         
         B     EFNF                                                             
         DROP  R6                                                               
*                                                                               
P1V14    DS    0H                                                               
         SR    RE,R0                                                            
         AHI   RE,1                RE=FILE NUMBER TO BEGIN DISPLAY              
         STCM  RE,15,SYSINDEX                                                   
         MVC   FIL,SYSFILE#                                                     
         ST    R5,FILADDR                                                       
         DROP  R5                                                               
*                                                                               
P1V16    CLI   PARM+4,3            P3 IS THE CURRENT EXTENT NUMBER              
         BL    P1VALX              YES                                          
*                                                                               
         AHI   R8,SCBLKLQ          NEXT FIELD IS EXTENT START                   
         MVC   FERRDSP,SC1STNUM                                                 
         CLI   SC2NDLEN,0                                                       
         BNE   EIIFF               NO SECOND PART TO ANY OF THE FIELDS          
         CLI   SC1STLEN,2                                                       
         BNE   ETOOL                                                            
*                                                                               
         TM    SC1STVAL,SCHEXQ     HEX DISPLAY START INDEX                      
         BZ    EIIFF                                                            
         GOTO1 AHEXIN,PARM,SC1STFLD,DUB,2                                       
         CLI   DUB,0                                                            
         BZ    EIIFF                                                            
         XR    RF,RF                                                            
         IC    RF,DUB                                                           
         STCM  RF,15,XTNINDEX                                                   
*                                                                               
P1VALX   XC    FERRDSP,FERRDSP                                                  
         LA    RE,SRVP2H                                                        
         ST    RE,CURSOR                                                        
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE P2                                                         *         
***********************************************************************         
P2VAL    NTR1                                                                   
         LA    R4,SRVP2H           P2=TYP/FMT FILTERS                           
         USING FHD,R4                                                           
         MVI   FILFMT,X'FF'        DEFAULT FORMAT                               
         CLI   FHIL,0                                                           
         BE    EXITOK                                                           
*                                                                               
         LA    R8,XNTINFO                                                       
         USING SCANBLKD,R8                                                      
         GOTO1 ASCANNER,PARM,SRVP2H,(X'82',(R8))                                
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EMISIF                                                           
*                                                                               
P2V02    MVC   FERRDSP,SC1STNUM                                                 
         CLI   SC1STLEN,3          FORMAT IS XXX=VALUE                          
         BNE   EIIFF                                                            
         CLI   SC2NDLEN,0                                                       
         BE    EIIFF                                                            
*                                                                               
         CLC   =C'TYP',SC1STFLD    TYP=XXX                                      
         BNE   P2V08                                                            
*                                                                               
         LA    RF,P2V2TBL                                                       
         CLI   SC2NDLEN,3                                                       
         BH    P2V06                                                            
*                                                                               
P2V04    CLI   0(RF),0             SEARCH TABLE OF VALID FILE TYPES             
         BE    P2V06                                                            
         CLC   SC2NDFLD(3),0(RF)                                                
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     P2V04                                                            
*                                                                               
         MVC   FILTYPE,3(RF)                                                    
         B     P2VA                                                             
*                                                                               
P2V06    MVI   XTRAMSGL,23                                                      
         MVC   XTRAMSG(23),=CL23'ALL/IS/DA/RCV/REQ/Q/REC'                       
         B     EKYWDV                                                           
*                                                                               
P2V08    CLC   =C'FMT',SC1STFLD    FMT=XXX                                      
         BNE   P2V14                                                            
         CLI   SC2NDLEN,3                                                       
         BH    P2V12                                                            
*                                                                               
         LA    RF,P2V3TBL                                                       
P2V10    CLI   0(RF),0             SEARCH TABLE OF VALID FORMATS                
         BE    P2V12                                                            
         CLC   SC2NDFLD(3),0(RF)                                                
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     P2V10                                                            
*                                                                               
         MVC   FILFMT,3(RF)                                                     
         B     P2VA                                                             
*                                                                               
P2V12    MVI   XTRAMSGL,18                                                      
         MVC   XTRAMSG(18),=CL18'F1/F2/P1/P2/B1/ISX'                            
         B     EKYWDV                                                           
*                                                                               
P2V14    MVC   XTRAMSGL,SC1STLEN                                                
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     EKYWD                                                            
         MVC   XTRAMSG(0),SC1STFLD                                              
*                                                                               
P2VA     AHI   R8,SCBLKLQ                                                       
         BCT   R0,P2V02                                                         
*                                                                               
*&&US                                                                           
         CLI   FILFMT,X'FF'        DEFAULT FORMAT?                              
         BE    P2VD                YES                                          
         TM    FILFMT,FMT_BIG      BIG FORMAT REQUESTED?                        
         BZ    P2VD                                                             
         CLI   SYS,DEMO_SYS        DEMO SYSTEM?                                 
         BNE   P2VD                                                             
         CLI   STINDFLG,C'Y'       SPECIFIC DEMO FILE PROVIDED?                 
         BNE   ERRDEM1                                                          
P2VD     DS    0H                                                               
*&&                                                                             
         LA    RE,SRVP3H                                                        
         ST    RE,CURSOR                                                        
         CLI   FILFMT,X'88'        TEST FORMAT TO DISPLAY I/S INDEX             
         BNE   EXITOK                                                           
         MVI   FILTYPE,TYPE_IS     SET FILE TYPE TO I/S                         
         B     EXITOK                                                           
         DROP  R4,R8                                                            
*                                                                               
P2V2TBL  DS    0CL4                FILE TYPE TABLE                              
         DC    C'ALL',X'00'                                                     
         DC    C'IS ',AL1(TYPE_IS)                                              
         DC    C'DA ',AL1(TYPE_DA)                                              
         DC    C'RCV',AL1(TYPE_RCV)                                             
         DC    C'REQ',AL1(TYPE_REQ)                                             
         DC    C'Q  ',AL1(TYPE_REQ)                                             
         DC    C'REC',AL1(TYPE_RCV)                                             
         DC    4X'00'                                                           
*                                                                               
TYPE_IS  EQU   X'01'               I/S FILE                                     
TYPE_DA  EQU   X'02'               D/A FILE                                     
TYPE_RCV EQU   X'40'               RECOVERY FILE                                
TYPE_REQ EQU   X'20'               REQUEST FILE                                 
*                                                                               
P2V3TBL  DS    0CL4                DISPLAY FORMAT TABLE                         
         DC    C'F1 ',X'00'                                                     
         DC    C'P1 ',AL1(FMT_LAST)                                             
         DC    C'F2 ',AL1(FMT_2UP)                                              
         DC    C'P2 ',AL1(FMT_LAST+FMT_2UP)                                     
         DC    C'B1 ',AL1(FMT_LAST+FMT_BIG)                                     
         DC    C'ISX',X'88'                                                     
         DC    4X'00'                                                           
*                                                                               
FMT_LAST EQU   X'80'               DISPLAY FIRST/LAST EXTENT ONLY               
FMT_2UP  EQU   X'40'               PARTIAL DISPLAY (2-UP)                       
FMT_RGHT EQU   X'20'               DISPLAY ON RIGHT SIDE OF SCREEN              
FMT_BIG  EQU   X'10'               DISPLAY BIG TOTALS                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE P3                                                         *         
***********************************************************************         
P3VAL    NTR1                                                                   
         MVI   HIGHXNT,16                                                       
         LA    R4,SRVP3H           P3=PCT/SYS FILTERS                           
         USING FHD,R4                                                           
         CLI   FHIL,0                                                           
         BE    EXIT                                                             
*                                                                               
         LA    R8,XNTINFO                                                       
         USING SCANBLKD,R8                                                      
         GOTO1 ASCANNER,PARM,(R4),(2,(R8))                                      
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EMISIF                                                           
*                                                                               
P3V02    MVC   FERRDSP,SC1STNUM                                                 
         CLI   SC1STLEN,3          FORMAT IS XXX=DATA                           
         BNE   EKYWD                                                            
         CLI   SC2NDLEN,0                                                       
         BE    EIIFF                                                            
*                                                                               
         CLC   =C'PCT',SC1STFLD    PCT=NNN WHERE NNN IS PERCENTAGE              
         BNE   P3V06                                                            
*                                                                               
         MVC   FERRDSP,SC2NDNUM                                                 
         TM    SC2NDVAL,SCNUMQ                                                  
         BZ    ENOTNM                                                           
         XR    R1,R1                                                            
         ICM   R1,7,SC2NDNUM+1                                                  
         BZ    EINPCT                                                           
         CHI   R1,100                                                           
         BH    EINPCT                                                           
         ST    R1,FILPCT                                                        
         B     P3V40                                                            
*                                                                               
P3V06    CLC   =C'CHU',SC1STFLD    CHU=HHH WHERE HHH IS HEX CHAN/UNIT           
         BNE   P3V08                                                            
*                                                                               
         CLI   SC2NDLEN,4                                                       
         BNE   EINCHN                                                           
         MVC   FERRDSP,SC2NDNUM                                                 
         TM    SC2NDVAL,SCHEXQ                                                  
         BZ    EINHEX                                                           
         GOTO1 AHEXIN,PARM,SC2NDFLD,FILSYSNO,4                                  
         OC    FILSYSNO(2),FILSYSNO                                             
         BZ    EINCHN                                                           
         XR    RF,RF                                                            
*NOP*    ICM   RF,3,FILSYSNO       CONVERT CHU0 TO 0CHU                         
*NOP*    SRL   RF,4                                                             
*NOP*    STCM  RF,3,FILSYSNO                                                    
         B     P3V40                                                            
*                                                                               
P3V08    CLC   =C'XTN',SC1STFLD    XTN=## WHERE HHH IS NUMBER 1 TO 16           
         BNE   P3V10                                                            
         CLI   SC2NDLEN,2                                                       
         BH    ETOOL                                                            
         TM    SC2NDVAL,SCNUMQ                                                  
         BZ    ENOTNM                                                           
         CLI   FLXNTS,16                                                        
         BH    ETOOL                                                            
         MVC   FLXNTS,SC2NDNUM+3                                                
         B     P3V40                                                            
*                                                                               
P3V10    CLC   =C'XTB',SC1STFLD    XTN=## WHERE HHH IS NUMBER 1 TO 48           
         BNE   EKYWD                                                            
         MVI   HIGHXNT,XNTMAX      48 FOR BIGDEMO                               
         CLI   SC2NDLEN,2                                                       
         BH    ETOOL                                                            
         TM    SC2NDVAL,SCNUMQ                                                  
         BZ    ENOTNM                                                           
         CLI   FLXNTS,XNTMAX                                                    
         BH    ETOOL                                                            
         MVC   FLXNTS,SC2NDNUM+3                                                
*                                                                               
P3V40    AHI   R8,SCBLKLQ                                                       
         BCT   R0,P3V02            NEXT ENTRY FOR P3                            
*                                                                               
         LA    RE,SRVP4H                                                        
         ST    RE,CURSOR                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE P4                                                         *         
***********************************************************************         
P4VAL    NTR1                                                                   
         LA    R4,SRVP4H           P4=PRINT QUEUE INFO                          
         USING FHD,R4                                                           
         ST    R4,CURSOR                                                        
         CLI   FHIL,0                                                           
         BE    P4VALX                                                           
*                                                                               
         MVC   APQBUFF,ATIA        USE TIA AS PRTQUE BUFFER                     
*&&UK*&& MVC   PRTUSER,=H'38'      U=DDS1                                       
*&&US*&& MVC   PRTUSER,=H'43'      U=TCH1                                       
         MVC   PRTID,=C'XTN'                                                    
         MVI   PRTCLASS,C'X'                                                    
         CLI   FHIL,2                                                           
         BNE   P4V02                                                            
         CLC   =C'PQ',FHDA         CAN INPUT PQ OR QU TO GET DEFAULTS           
         BE    P4VALX                                                           
         CLC   =C'QU',FHDA                                                      
         BE    P4VALX                                                           
         B     EPQID                                                            
*                                                                               
P4V02    LA    R8,XNTINFO          U=USERID,CLASS,REPID                         
         USING SCANBLKD,R8                                                      
         GOTO1 ASCANNER,PARM,FHD,(X'86',(R8))                                   
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)          R5=NUMBER OF INPUT FIELDS                    
         BZ    EIIFF                                                            
         CHI   R5,3                                                             
         BH    ETMIF                                                            
*                                                                               
         CLI   SC1STLEN,1          FIRST PARM IS U=PQUSERID                     
         BNE   EPQID                                                            
         CLI   SC1STFLD,C'U'                                                    
         BNE   EPQID                                                            
         CLI   SC2NDLEN,3                                                       
         BL    EPQID                                                            
*                                                                               
         LA    R7,IO               READ USER ID REC                             
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SC2NDFLD                                                  
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,CTIREC,CTIREC                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         MVC   FERRDSP,SC2NDNUM                                                 
         B     EINUSR                                                           
*                                                                               
         LA    R7,CTIDATA          SEARCH FOR ID NUMBER ELEMENT                 
         USING CTDSCD,R7                                                        
         XR    RF,RF                                                            
P4V04    CLI   CTDSCEL,0                                                        
         BE    EPQID                                                            
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R7,RF,P4V04                                                      
*                                                                               
         MVC   PRTUSER,CTDSC       SET OVERRIDE PQ USER ID NUMBER               
         AHI   R8,SCBLKLQ                                                       
         CHI   R0,1                                                             
         BE    P4VALX                                                           
         DROP  R7                                                               
*                                                                               
         MVC   FERRDSP,SC2NDNUM    OPTIONAL 1 CHR CLASS                         
         CLI   SC2NDLEN,0                                                       
         BNE   EIIFF                                                            
         CLI   SC1STLEN,1                                                       
         BNE   P4V06                                                            
*                                                                               
         MVC   FERRDSP,SC1STNUM                                                 
         TM    SC1STVAL,SCALPHAQ                                                
         BZ    ECLASS                                                           
         MVC   PRTCLASS,SC1STFLD   SINGLE CHR IS CLASS FILTER                   
         AHI   R8,SCBLKLQ                                                       
*                                                                               
         CHI   R0,3                                                             
         BNE   P4VALX                                                           
*                                                                               
P4V06    MVC   FERRDSP,SC1STNUM                                                 
         CHI   R0,3                                                             
         BNE   EIIFF                                                            
*                                                                               
         MVC   FERRDSP,SC2NDNUM    SUBID MUST BE 2/3 CHRS                       
         CLI   SC2NDLEN,0                                                       
         BNE   EIIFF                                                            
*                                                                               
         MVC   FERRDSP,SC1STNUM                                                 
         CLI   SC1STLEN,3                                                       
         BH    EREPID                                                           
         CLI   SC1STLEN,2                                                       
         BL    EREPID                                                           
*                                                                               
         MVC   PRTID,SC1STFLD                                                   
         LA    RE,PRTID                                                         
         LHI   RF,3                                                             
P4V08    CLI   0(RE),C'A'          MASSAGE SUBID                                
         BNL   *+8                                                              
         MVI   0(RE),C'.'                                                       
         AHI   RE,1                                                             
         BCT   RF,P4V08                                                         
*                                                                               
P4VALX   XC    FERRDSP,FERRDSP                                                  
         LA    RF,SRVHL1H                                                       
         ST    RF,CURSOR                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR PRINT QUEUE DISPLAY                                  *         
***********************************************************************         
PRTFST   NTR1                                                                   
         MVI   FILFMT,0            FORMAT=F1 (FULL/1UP)                         
         LA    R4,P                                                             
         USING PQPLD,R4                                                         
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLSRCID,PRTUSER                                                  
         MVC   QLSUBID,PRTID                                                    
         MVC   QLCLASS,PRTCLASS                                                 
         MVC   QLDESC,=CL11'$XTNT/     '                                        
         L     RE,ASYSFACS                                                      
         L     RE,VSSB-SYSFACD(RE)                                              
         MVC   QLDESC+6(3),SSBSYSNA-SSBD(RE)                                    
         CLI   SYS,0                                                            
         BE    *+10                                                             
         MVC   QLDESC+6(5),SYSNAME                                              
         MVC   QLMAKER,=C'DSR  '                                                
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,80                                                       
         MVC   QLRETNL,=H'48'                                                   
         MVC   QLRETND,=H'24'                                                   
         GOTO1 ADATAMGR,DMCB,DMPRINT,PRTQUE,0,(R4),APQBUFF                      
         CLI   8(R1),0                                                          
         BNE   EPQDSK                                                           
         MVC   PRTREPNO,QLREPRNO   SAVE RETURNED REPORT NUMBER                  
         MVC   PRTID,QLSUBID       SAVE RETURNED REPORT ID                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SINGLE FILE DISPLAY LOGIC                                           *         
***********************************************************************         
FILDSP   NTR1                                                                   
         LA    RF,SRVP2H                                                        
         ST    RF,CURSOR                                                        
*                                                                               
         MVI   FMT,0               SET FULL FORMAT                              
*NOP*    L     R1,FILADDR                                                       
*NOP*    BAS   RE,GETXTNT                                                       
*NOP*    BE    EXTNT                                                            
*                                                                               
         OC    PRTUSER,PRTUSER     WANT PRINTOUT?                               
         BZ    *+8                 NO                                           
         BRAS  RE,PRTXTNT          PRINT IT OUT IF NECESSARY                    
         LA    R1,SRVDL1H                                                       
         BRAS  RE,SYSDSP                                                        
*                                                                               
         LH    R1,FXNTS            WILL FILE FIT ON SCREEN?                     
         C     R1,MAXLINES                                                      
         BNH   FDSP08                                                           
         A     R0,XTNINDEX         NO                                           
         ST    R0,FULL             SET NEXT START NUMBER                        
         CH    R0,FXNTS                                                         
         BNE   *+10                                                             
         XC    FULL,FULL                                                        
*                                                                               
         LA    R8,XNTINFO                                                       
         USING SCANBLKD,R8                                                      
         GOTO1 ASCANNER,PARM,SRVP1H,(X'86',XNTINFO)                             
         XR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         CHI   R0,2                                                             
         BL    *+8                                                              
         LHI   R0,2                                                             
*                                                                               
         LA    R6,XNTINFO                                                       
FDSP04   MVC   0(20,R6),SCONEFLD                                                
         AHI   R8,SCBLKLQ                                                       
         AHI   R6,20                                                            
         BCT   R0,FDSP04                                                        
         MVC   0(20,R6),SPACES                                                  
*                                                                               
         LHI   R0,2                                                             
         OC    FULL,FULL                                                        
         BZ    FDSP06                                                           
         GOTO1 AHEXOUT,DMCB,FULL+3,0(R6),1,=C'TOG'                              
         LHI   R0,3                                                             
*                                                                               
FDSP06   GOTO1 AUNSCAN,PARM,((R0),XNTINFO),SRVP1H                               
*                                                                               
FDSP08   MVC   MSG(22),=C'File extents displayed'                               
         LHI   R1,22                                                            
         B     OKX                                                              
         EJECT                                                                  
***********************************************************************         
* SET FILE DEFINITIONS IF NOT ALREADY SET                             *         
***********************************************************************         
SYSDEF   NTR1                                                                   
         CLI   SYS,0               SET DEFAULTS FOR SINGLE SYSTEM               
         BE    SYSDEF1                                                          
         CLI   FILFMT,X'FF'                                                     
         BNE   *+8                                                              
         MVI   FILFMT,0            FORMAT=F1 (FULL/1UP)                         
         B     SYSDEF2                                                          
*                                                                               
SYSDEF1  DS    0H                  SET DEFAULTS FOR NO INPUT                    
*                                                                               
SYSDEF2  CLI   FILFMT,X'FF'        SET DEFAULTS FOR ALL SYSTEMS                 
         BNE   *+8                                                              
         MVI   FILFMT,X'C0'        FORMAT=P2 (PART/2UP)                         
*                                                                               
         TM    FILFMT,FMT_2UP      ADJUST HEADLINES FOR 2UP FORMAT              
         BZ    EXIT                                                             
         MVC   SRVHL1+40(39),SRVHL1                                             
         MVC   SRVHL2+40(39),SRVHL2                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY SYSTEM INFORMATION                                          *         
***********************************************************************         
SYSDSP   NTR1                                                                   
         L     R5,SYSADDR          LOAD R5 HERE JUST INCASE WE PRINT            
         OC    PRTUSER,PRTUSER     WANT PRINTOUT?                               
         BZ    *+8                 NO                                           
         BRAS  RE,PRTDSP                                                        
         ST    R5,SYSADDR          PRTDSP DESTROYS SYSADDR, SO RESTORE          
*                                                                               
         MVC   FMT,FILFMT          SET DISPLAY FORMAT                           
*nop*    MVC   DSPLINES,MAXLINES                                                
         XC    DSPINDEX,DSPINDEX                                                
*                                                                               
         XC    DSPLINDX,DSPLINDX                                                
         LA    R8,SRVDL1H          R8=A(NEXT SCR DSP LINE HDR)                  
         USING FHD,R8                                                           
LOOP     L     R5,SYSADDR          BUMP TO NEXT SYSTEM                          
         BRAS  RE,VALSYS                                                        
         BNE   SYSDSP1A                                                         
*                                                                               
         L     R5,SYSADDR          START OF NEXT SYSTEM                         
         USING SYSFLSTD,R5                                                      
*&&US                                                                           
         CLI   SYSFSYS#,DEMO_SYS   ONLY SHOW DEMO FILES IF P1 IS...             
         BNE   *+12                EXPLICITLY SET TO DEMO                       
         CLI   SYS,DEMO_SYS                                                     
         BNE   SYSDSP1A                                                         
*&&                                                                             
         MVC   HALF(1),SYSFSYS#    SET SE NUMBER                                
         LLH   R6,SYSF#FLS         R6=NUM OF FILES IN SYSTEM                    
         LA    R5,SYSFLIST         START OF LIST                                
         CLI   STINDFLG,C'Y'                                                    
         BNE   SYSDSP5                                                          
         S     R6,SYSINDEX         Display starting at this file                
         L     R1,SYSINDEX                                                      
         SHI   R1,1                                                             
         ST    R1,DSPINDEX                                                      
         L     R5,FILADDR                                                       
*                                                                               
SYSDSP5  MVC   HALF+1(1),SYSFILE#  FILE NUMBER                                  
         LR    R1,R5                                                            
         TM    SYSFIND2,SFALIAS    BYPASS ALIAS FILES                           
         BO    SYSDSP6                                                          
*                                                                               
         BAS   RE,GETXTNT                                                       
         BE    SYSDSP6             BYPASS IF NO EXTENTS                         
         BAS   RE,TSTSYN                                                        
         BE    SYSDSP6             BYPASS IF SYNONYM                            
         BAS   RE,TSTTYP                                                        
         BNE   SYSDSP6             BYPASS IF NOT FILE TYPE                      
         BAS   RE,TSTSYS                                                        
         BNE   SYSDSP6             BYPASS IF NOT SAME SYSNUM                    
         BAS   RE,TSTPCT                                                        
         BNE   SYSDSP6             BYPASS IF PCT AVAIL GT INPUT                 
*                                                                               
         L     R1,DSPINDEX                                                      
         AHI   R1,1                                                             
         ST    R1,DSPINDEX         BUMP INDEX OF FILES DISPLAYABLE              
         C     R1,SYSINDEX                                                      
         BNL   SYSDSP7                                                          
*                                                                               
SYSDSP6  LA    R5,SYSFLNQ(R5)      BUMP TO NEXT FILE                            
         BCT   R6,SYSDSP5                                                       
         B     SYSDSP1A                                                         
*                                                                               
SYSDSP7  LR    R1,R8               DISPLAY FILE EXTENTS                         
         BRAS  RE,DSPXTNT                                                       
         LR    R8,R1                                                            
*                                                                               
         ST    R0,MAXXTNT          NUMBER DISPLAYED ON THIS ONE                 
         MVC   DSPLINDX,DSPINDEX   SAVE INDEX OF LAST FILE DISPLAYED            
*&&US                                                                           
         TM    FMT,FMT_BIG         BIG TOTALS?                                  
         BZ    *+12                NO                                           
         CLI   SYS,DEMO_SYS        DEMO SYSTEM?                                 
         BE    SYSDSPA             YES ONLY SHOW ONE DEMO FILE                  
*&&                                                                             
         CLI   FHLN,0              REACHED END OF SCREEN?                       
         BE    SYSDSPA                                                          
         XC    MAXXTNT,MAXXTNT     NO                                           
         B     SYSDSP6                                                          
*                                                                               
SYSDSP1A L     R5,SYSADDR          BYPASS THIS SYSTEM                           
         LLH   R6,SYSF#FLS         NUMBER OF FILES                              
         MHI   R6,SYSFLNQ                                                       
         LA    R5,SYSFLIST(R6)     POINT TO END OF SYSTEM FILE LIST             
         ST    R5,SYSADDR          GOTO END OF SYSTEM                           
*                                                                               
         XR    R6,R6               FOR DISPLAY MESSAGE BELOW                    
         XC    XTNINDEX,XTNINDEX                                                
         CLI   SYS,0                                                            
         BNE   SYSDSPA             END OF SINGLE SYSTEM                         
         CLI   0(R5),X'FF'                                                      
         BE    SYSDSPA             END OF ALL SYSTEMS                           
         B     LOOP                                                             
*                                                                               
SYSDSPA  OC    DSPLINDX,DSPLINDX                                                
         BNZ   SYSDSPB                                                          
         MVC   MSG(14),=C'No files found'                                       
         LA    R1,14                                                            
         B     OKX                                                              
*                                                                               
SYSDSPB  OC    MAXXTNT,MAXXTNT     DISPLAYED ALL OF THIS FILE?                  
         BNZ   SYSDSPC             NO                                           
*                                                                               
         LTR   R6,R6               DISPLAYED LAST FILE?                         
         BNZ   SYSDSPC                                                          
*                                                                               
         MVC   MSG(22),=C'File extents displayed'                               
         CLI   SRVP1H+5,0                                                       
         BE    SYSDSPB1                                                         
         BAS   RE,NONXTFL                                                       
SYSDSPB1 LHI   R1,22                                                            
         B     OKX                                                              
*                                                                               
SYSDSPC  MVC   MSG(45),SYSDSPM                                                  
         L     R0,SYSINDEX                                                      
         BAS   RE,DSPCVD                                                        
         MVC   MSG+13(4),DUB+4                                                  
         L     R0,DSPLINDX                                                      
         BAS   RE,DSPCVD                                                        
         MVC   MSG+23(4),DUB+4                                                  
         L     R0,DSPINDEX                                                      
         BAS   RE,DSPCVD                                                        
         MVC   MSG+31(4),DUB+4                                                  
         BAS   RE,NXTFILE          INSERT THE (,FILE)                           
         LHI   R1,42                                                            
         B     OKX                                                              
SYSDSPM  DC    CL45'File extents NNNN thru NNNN of NNNN displayed'              
*                                                                               
*NOP*    L     R5,SYSADDR          BUMP TO NEXT SYSTEM                          
*NOP*    XR    R6,R6                                                            
*NOP*    LLH   R6,SYSF#FLS                                                      
*NOP*    MHI   R6,SYSFLNQ                                                       
*NOP*    LA    R5,SYSFLIST(R6)     POINT TO END OF SYSTEM FILE LIST             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THIS SYSTEM IS OK TO DISPLAY                               *         
***********************************************************************         
VALSYS   NTR1                                                                   
         L     R5,SYSADDR          TEST IF NEXT SYSTEM QUALIFIES                
         USING SYSFLSTD,R5                                                      
         MVC   HALF(1),SYSFSYS#                                                 
         DROP  R5                                                               
                                                                                
         L     R5,ASYSFACS                                                      
         L     R5,VSELIST-SYSFACD(R5) SEARCH SELIST FOR SYSTEM                  
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         CLC   SESYS,HALF                                                       
         BE    *+12                                                             
         BXLE  R5,R6,*-10                                                       
         B     EXITL                                                            
*                                                                               
         CLI   RW,C'+'             TEST IF UPDATIVE SYSTEMS ONLY                
         BE    *+12                                                             
         CLI   RW,C'U'                                                          
         BNE   *+12                                                             
         TM    SEIND,SEIRONLY+SEISETRO                                          
         BNZ   EXITL                                                            
*                                                                               
         TM    SEIND,SEIRESA       SYSTEM MUST BE OPEN                          
         BO    *+12                                                             
         TM    SEIND,SEISTRT                                                    
         BZ    EXITL                                                            
*                                                                               
         OC    SEFILES,SEFILES     SYSTEM MUST HAVE FILES                       
         BZ    EXITL                                                            
         MVC   SYSNAME,SENAME      SAVE SYSTEM NAME                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT SYSTEM INFORMATION                                            *         
***********************************************************************         
PRTDSP   NTR1                                                                   
         MVC   FMT,FILFMT          SET DISPLAY FORMAT                           
         OI    SRVP4H+6,FHOITR                                                  
         MVC   SRVP4,SPACES        CLEAR SO WE DON'T PRINT REPEATEDLY           
*                                                                               
PDSP02   L     R5,SYSADDR          TEST IF NEXT SYSTEM QUALIFIES                
         USING SYSFLSTD,R5                                                      
         BAS   RE,VALSYS                                                        
*&&UK*&& BE    PDSP06                                                           
*&&US                                                                           
         BNE   PDSP04              NEXT SYSTEM                                  
         CLI   SYSFSYS#,DEMO_SYS   ONLY SHOW DEMO FILES IF P1 IS...             
         BNE   PDSP06              EXPLICITLY SET TO DEMO                       
         CLI   SYS,DEMO_SYS                                                     
         BE    PDSP06              OKAY TO DISPLAY                              
*&&                                                                             
PDSP04   L     R5,SYSADDR          BYPASS THIS SYSTEM                           
         LLH   R6,SYSF#FLS                                                      
         MHI   R6,SYSFLNQ                                                       
         LA    R5,SYSFLIST(R6)     POINT TO END OF SYSTEM FILE LIST             
         B     PDSP14              GO TO END OF SYSTEM                          
         DROP  R5                                                               
*                                                                               
PDSP06   MVI   P,X'89'             PRINT SKIP TO NEW PAGE                       
         MVC   PLINE,SPACES                                                     
         GOTO1 ADATAMGR,DMCB,DMPRINT,PRTQUE,0,P,APQBUFF                         
         CLI   8(R1),0                                                          
         BNE   EPQDSK                                                           
*                                                                               
         MVI   P,X'09'             PRINT HEADLINE ONE                           
         MVC   PLINE,SPACES                                                     
         MVC   PLINE(L'SRVHL1),SRVHL1                                           
         MVC   PLINE(L'SYSNAME),SYSNAME                                         
         GOTO1 ADATAMGR,DMCB,DMPRINT,PRTQUE,0,P,APQBUFF                         
         CLI   8(R1),0                                                          
         BNE   EPQDSK                                                           
*                                                                               
         MVI   P,X'09'             PRINT HEADLINE TWO                           
         MVC   PLINE,SPACES                                                     
         MVC   PLINE(L'SRVHL2),SRVHL2                                           
         GOTO1 ADATAMGR,DMCB,DMPRINT,PRTQUE,0,P,APQBUFF                         
         CLI   8(R1),0                                                          
         BNE   EPQDSK                                                           
         MVC   PLINE,SPACES        RESET PLINE                                  
*                                                                               
         L     R5,SYSADDR          START OF NEXT SYSTEM                         
         USING SYSFLSTD,R5                                                      
         MVC   HALF(1),SYSFSYS#    SET SE NUMBER                                
         LLH   R6,SYSF#FLS         R6=NUM OF FILES IN SYSTEM                    
         LA    R5,SYSFLIST                                                      
*                                                                               
PDSP10   MVC   HALF+1(1),SYSFILE#  FILE NUMBER                                  
         LR    R1,R5                                                            
         TM    SYSFIND2,SFALIAS    BYPASS ALIAS FILES                           
         BO    PDSP12                                                           
*                                                                               
         BAS   RE,GETXTNT                                                       
         BE    PDSP12              BYPASS IF NO EXTENTS                         
         BAS   RE,TSTSYN                                                        
         BE    PDSP12              BYPASS IF SYNONYM                            
         BAS   RE,TSTTYP                                                        
         BNE   PDSP12              BYPASS IF NOT FILE TYPE                      
         BAS   RE,TSTSYS                                                        
         BNE   PDSP12              BYPASS IF NOT SAME SYSNUM                    
         BAS   RE,TSTPCT                                                        
         BNE   PDSP12              BYPASS IF PCT AVAIL GT INPUT                 
*                                                                               
         L     R1,DSPINDEX                                                      
         AHI   R1,1                                                             
         ST    R1,DSPINDEX         BUMP INDEX OF FILES DISPLAYABLE              
         C     R1,SYSINDEX                                                      
         BL    PDSP12              BYPASS IF LESS THAN INPUT VALUE              
*                                                                               
         BRAS  RE,PRTXTNT          PRINT IT OUT IF NECESSARY                    
*                                                                               
PDSP12   LA    R5,SYSFLNQ(R5)      BUMP TO NEXT FILE                            
         BCT   R6,PDSP10                                                        
*                                                                               
PDSP14   ST    R5,SYSADDR          BUMP TO NEXT SYSTEM                          
         CLI   SYS,0                                                            
         BNE   EXITOK              END OF SINGLE SYSTEM                         
         CLI   0(R5),X'FF'                                                      
         BNE   PDSP02                                                           
         B     EXITOK              END OF ALL SYSTEMS                           
         EJECT                                                                  
***********************************************************************         
* WRITE EXTENTS INFORMATION TO A REPORT ON THE PQ                     *         
***********************************************************************         
         PUSH  USING                                                            
         USING XNTDISPD,PLINE                                                   
PRTXTNT  NTR1                                                                   
         MVI   FSTLINE,C'Y'                                                     
*                                                                               
         LA    R5,XNTINFO          R5=A(NEXT XNT DATA)                          
         USING XNTINFOD,R5                                                      
         ICM   RF,15,XTNINDEX                                                   
         MHI   RF,L'XNTINFO                                                     
         AR    R5,RF                                                            
*                                                                               
         LH    R0,FXNTS                                                         
         S     R0,XTNINDEX                                                      
         XR    R6,R6               COUNT NUMBER OF LINES                        
*                                                                               
PRTX02   MVC   PLINE,SPACES                                                     
         GOTO1 AHEXOUT,PARM,XNUM,XDCHU,1,=C'TOG'                                
         MVI   XDCHU+2,C'/'                                                     
         MVC   XDCHU+3(4),=C'VSAM'                                              
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    PRTX03                                                           
         GOTO1 AHEXOUT,PARM,XSYS,XDCHU+3,L'XSYS,=C'TOG'                         
*                                                                               
PRTX03   MVC   XDCCHH(5),DOTS                                                   
         MVI   XDCCHH+5,C'-'                                                    
         MVC   XDCCHH+6(5),DOTS                                                 
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    PRTX03A                                                          
         LH    RF,XLOW             CCCCC-CCCCC                                  
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDCCHH+0(5),DUB                                                  
         LH    RF,XHIGH                                                         
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDCCHH+6(5),DUB                                                  
*                                                                               
PRTX03A  MVC   XDSIZE,DOTS                                                      
         MVC   XDINUSE,DOTS                                                     
         MVC   XDAVAIL,DOTS                                                     
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    PRTX06                                                           
*                                                                               
         L     RF,XSIZE            SIZE OF EXTENT                               
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDSIZE,DUB                                                       
*                                                                               
         TM    XINUSE,X'80'        INUSE COUNT                                  
         BO    PRTX04                                                           
         L     RF,XINUSE                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDINUSE,DUB                                                      
*                                                                               
PRTX04   TM    XAVAIL,X'80'        AVAIL COUNT                                  
         BO    PRTX06                                                           
         L     RF,XAVAIL                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDAVAIL,DUB                                                      
*                                                                               
PRTX06   GOTOR DFILSTAT,XDFILE                                                  
*                                                                               
PRTX08   MVC   XDLAST,DOTS         DISK ADDRESS OF LAST RECORD                  
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    PRTX10                                                           
         CLC   XLAST,FFS                                                        
         BE    PRTX10                                                           
         GOTO1 AHEXOUT,PARM,XLAST,XDLAST,4,=C'TOG'                              
*                                                                               
PRTX10   CLI   FSTLINE,C'Y'        STILL DISPLAYING FIRST LINE                  
         BNE   PRTX14                                                           
*                                                                               
         MVC   XDNAME,FNAME                                                     
         MVC   XDPCT,DOTS                                                       
         TM    FPCT,X'80'                                                       
         BO    PRTX12                                                           
         L     RF,FPCT                                                          
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDPCT,DUB           SET DA DATA PCT                              
*                                                                               
PRTX12   LA    RF,FDTF+1                                                        
         GOTO1 AHEXOUT,PARM,(RF),XDDTF,3,=C'TOG'                                
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FDEV                                                          
         MHI   RE,8                                                             
         LA    RE,DEVINFO(RE)                                                   
         MVC   XDDEV,0(RE)                                                      
*                                                                               
PRTX14   MVI   P,X'09'                                                          
         GOTO1 ADATAMGR,DMCB,DMPRINT,PRTQUE,0,P,APQBUFF                         
         CLI   8(R1),0                                                          
         BNE   EPQDSK                                                           
*                                                                               
         MVI   FSTLINE,C'N'        SET NOT FIRST LINE                           
         AHI   R5,L'XNTINFO        BUMP TO NEXT EXTENT                          
         AHI   R6,1                ADD ANOTHER LINE                             
         BCT   R0,PRTX02                                                        
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY NEXT START NUMBER                                *         
***********************************************************************         
NXTFILE  NTR1                                                                   
         L     R0,MAXXTNT                                                       
         A     R0,XTNINDEX                                                      
         ST    R0,FULL             SET NEXT START NUMBER                        
*                                                                               
         LA    R8,XNTINFO                                                       
         USING SCANBLKD,R8                                                      
         GOTO1 ASCANNER,PARM,SRVP1H,(X'86',XNTINFO)                             
*                                                                               
         LA    R6,XNTINFO                                                       
         MVC   0(20,R6),SCONEFLD                                                
         AHI   R6,20                                                            
         MVC   0(40,R6),SPACES     THIS IS DELIBERATE - 2 PARMS                 
*                                                                               
         MVI   THRDNUM,C'N'        ONLY IF MORE EXTENTS THAN FIT                
         LH    RF,FXNTS                                                         
         TM    FMT,FMT_LAST        POSITION TO LAST DA EXTENT                   
         BZ    *+8                                                              
         LHI   RF,1                                                             
         C     RF,FULL             NUMBER DISPLAYED ON THIS ONE                 
         BE    *+8                                                              
         MVI   THRDNUM,C'Y'        REQUIRE THIRD PARAMETER?                     
*                                                                               
         L     R0,DSPLINDX                                                      
         CLI   THRDNUM,C'Y'        REQUIRE THIRD PARAMETER?                     
         BE    *+8                                                              
         AHI   R0,1                                                             
*                                                                               
         BAS   RE,DSPCVD                                                        
         MVC   0(4,R6),DUB+4       SET NEXT FILE NUMBER TO DISPLAY              
         AHI   R6,20                                                            
*                                                                               
         LHI   R0,2                                                             
         CLI   THRDNUM,C'Y'                                                     
         BNE   NXTFL02                                                          
         ICM   RF,15,FULL          MAKE DFS THIS IS NON-ZERO                    
         BZ    NXTFL02                                                          
*                                                                               
         GOTO1 AHEXOUT,DMCB,FULL+3,0(R6),1,=C'TOG'                              
         LHI   R0,3                                                             
*                                                                               
NXTFL02  MVC   SRVP1,SPACES                                                     
         GOTO1 AUNSCAN,PARM,((R0),XNTINFO),SRVP1H                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SHOW THAT THERE ARE NO MORE FILES TO DISPLAY             *         
***********************************************************************         
NONXTFL  NTR1                                                                   
         LA    R4,SRVP1H                                                        
         USING FHD,R4                                                           
         CLI   FHIL,0              LENGTH OF FIRST PARAMETER                    
         BE    EXIT                                                             
*                                                                               
         LA    R8,XNTINFO                                                       
         USING SCANBLKD,R8                                                      
         GOTO1 ASCANNER,PARM,SRVP1H,(X'84',XNTINFO)                             
*                                                                               
         LA    R6,XNTINFO                                                       
         MVC   0(20,R6),SCONEFLD                                                
         AHI   R6,20                                                            
         MVC   0(20,R6),SPACES                                                  
         MVC   0(20,R6),=CL3'001'                                               
         MVC   SRVP1,SPACES                                                     
         GOTO1 AUNSCAN,PARM,(2,XNTINFO),SRVP1H                                  
         OI    FHII,FHIITH         INPUT THIS TIME                              
         LA    RE,SRVIDH                                                        
         ST    RE,CURSOR           POSITION CURSOR TO SHOW THATS ALL            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR CODES                                                         *         
***********************************************************************         
SR$MISIF EQU   0001                MISSING INPUT FIELD                          
SR$INVFL EQU   0002                INVALID INPUT FIELD                          
SR$NOTNM EQU   0003                NOT A NUMBER                                 
SR$INHEX EQU   0005                INVALID HEX                                  
SR$TMIF  EQU   0010                TOO MANY INPUT FIELDS                        
SR$KYWD  EQU   0012                INVALID KEYWORD                              
SR$IIFF  EQU   0018                INVALID INPUT FIELD FORMAT                   
SR$INUSR EQU   0031                INVALID USER-ID                              
SR$INSYS EQU   0032                INVALID SYSTEM                               
SR$REPID EQU   0036                INVALID REPORT ID                            
SR$KYWDV EQU   0049                INVALID KEYWORD VALUE                        
SR$TOOL  EQU   0060                INPUT TOO LONG                               
SR$XTNT  EQU   0081                BAD EXTENT MATRIX                            
SR$INPCT EQU   0082                INVALID PERCENTAGE                           
SR$INCHN EQU   0083                INVALID CHANNEL/UNIT                         
SR$PQID  EQU   0084                PRINT QUEUE ID NOT U=USERID(,C),RID          
SR$SYSNO EQU   0102                SYSTEM NOT OPEN                              
SR$PQDSK EQU   0102                PRINT QUEUE DISK ERROR                       
SR$CLASS EQU   0180                INVALID CLASS                                
SR$IFID  EQU   0282                INVALID FILE ID                              
SR$FNF   EQU   0290                FILE NOT FOUND                               
*                                                                               
EMISIF   LHI   R0,SR$MISIF         MISSING INPUT FIELD                          
         B     ERROR                                                            
EPQDSK   LHI   R0,SR$PQDSK         PRINT QUEUE ERROR                            
         B     ERROR                                                            
EREPID   LHI   R0,SR$REPID         INVALID REPORT ID                            
         B     ERROR                                                            
ECLASS   LHI   R0,SR$CLASS         INVALID CLASS                                
         B     ERROR                                                            
EINCHN   LHI   R0,SR$INCHN         INVALID CHANNEL/UNIT                         
         B     ERROR                                                            
EINHEX   LHI   R0,SR$INHEX         INVALID HEX                                  
         B     ERROR                                                            
ENOTNM   LHI   R0,SR$NOTNM         NOT NUMERIC                                  
         B     ERROR                                                            
EINPCT   LHI   R0,SR$INPCT         INVALID PERCENTAGE                           
         B     ERROR                                                            
EMINVFL  LHI   R0,SR$INVFL         INVALID INPUT FIELD                          
         B     ERROR                                                            
ETMIF    LHI   R0,SR$TMIF          TOO MANY INPUT FIELDS                        
         B     ERROR                                                            
EIIFF    LHI   R0,SR$IIFF          INVALID INPUT FIELD FORMAT                   
         B     ERROR                                                            
EXTNT    LHI   R0,SR$XTNT          BAD EXTENT MATRIX                            
         B     ERROR                                                            
EKYWD    LHI   R0,SR$KYWD          INVALID KEYWORD                              
         B     ERROR                                                            
EKYWDV   LHI   R0,SR$KYWDV         INVALID KEYWORD VALUE                        
         B     ERROR                                                            
EINSYS   LHI   R0,SR$INSYS         INVALID SYSTEM                               
         B     ERROR                                                            
EINUSR   LHI   R0,SR$INUSR         INVALID USER-ID                              
         B     ERROR                                                            
ESYSNO   LHI   R0,SR$SYSNO         SYSTEM NOT OPEN                              
         B     ERROR                                                            
EIFID    LHI   R0,SR$IFID          INVALID FILE ID                              
         B     ERROR                                                            
EFNF     LHI   R0,SR$FNF           FILE NOT FOUND                               
         B     ERROR                                                            
ETOOL    LHI   R0,SR$TOOL          INPUT TOO LONG                               
         B     ERROR                                                            
EPQID    LHI   R0,SR$PQID          PRINT QUEUE ID NOT U=USERID(,C),RID          
         B     ERROR                                                            
*                                                                               
ERRDEM1  MVC   MSG(43),=C'B1 format only valid for a single DEMO file'          
         B     ERRX                                                             
*                                                                               
ERRX     MVC   SRVMSG(8),=C'ED-0000 '                                           
         MVC   SRVMSG+8(48),MSG                                                 
         OI    6(R4),X'40'                                                      
         B     XMOD                                                             
*                                                                               
ERROR    LA    R1,GTB              POINT TO GETTXT BLOCK                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FNDX         SET FIELD NUMBER                             
         MVC   GTSUBX,FSUB         SET FIELD REASON CODE                        
         STH   R0,GTMSGNO          SET ERROR NUMBER                             
*                                                                               
         CLI   XTRAMSGL,0                                                       
         BE    ERROR2                                                           
*                                                                               
         LA    RE,XTRAMSG          IF YOU EVER NEED TO ADD TEXT TO END          
         STCM  RE,7,GTATXT                                                      
         MVC   GTLTXT,XTRAMSGL                                                  
*                                                                               
ERROR2   MVI   GTMTYP,GTMERR       SET ERROR MESSAGE                            
         MVI   GTMSYS,1            SET SERVICE SYSTEM MESSAGES                  
         GOTO1 AGETTXT,(R1)                                                     
         DROP  R1                                                               
*                                                                               
XIT      ICM   R2,15,CURSOR        SET CURSOR ON BAD FIELD                      
         BZ    XMOD                                                             
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R1,R2                                                            
         SR    R1,R3                                                            
         STCM  R1,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     XMOD                                                             
         DROP  R2,RF                                                            
                                                                                
OKX      MVC   SRVMSG,MSG          OUTPUT OK MESSAGE                            
         OC    PRTUSER,PRTUSER     REPORT ON PRINT QUEUE                        
         BZ    XIT                                                              
*                                                                               
         LA    R5,MSG(R1)          POINT TO END OF MSG                          
         MVC   0(3,R5),=C' - '                                                  
         MVC   3(3,R5),PRTID       OUTPUT REPORT ID AS XXX,NNNNN                
         MVI   6(R5),C','                                                       
         LA    R5,7(R5)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,PRTREPNO                                                    
         EDIT  (R0),(5,(R5)),ALIGN=LEFT                                         
         MVC   SRVMSG,MSG                                                       
*                                                                               
         MVI   P,X'FF'        CLOSE REPORT                                      
         MVC   PLINE,SPACES                                                     
         GOTO1 ADATAMGR,DMCB,DMPRINT,PRTQUE,0,P,APQBUFF                         
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         XC    MSG,MSG                                                          
         B     EPQDSK                                                           
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
XMOD     L     RD,SAVERD                                                        
         XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* TEST IF SYS/FIL IN HALF BELONGS TO SYSTEM OR IS A SYNONYM           *         
***********************************************************************         
TSTSYN   NTR1                                                                   
         XR    R1,R1                                                            
         IC    R1,HALF                                                          
         MHI   R1,4                                                             
         LA    R1,SYSTAB(R1)                                                    
         ICM   RF,15,0(R1)                                                      
         A     RF,RELO                                                          
*                                                                               
TSYN02   CLI   0(RF),X'FF'                                                      
         BE    EXITL                                                            
         CLC   0(1,RF),HALF+1                                                   
         BE    EXITOK                                                           
         AHI   RF,1                                                             
         B     TSYN02                                                           
         EJECT                                                                  
***********************************************************************         
* TEST IF FILE BELONGS TO A FILE TYPE                                 *         
***********************************************************************         
TSTTYP   NTR1                                                                   
         CLI   FILTYPE,0                                                        
         BE    EXITOK              NO TYPE INPUT SO INCLUDE                     
         MVC   FILEFLAG,FTYPE                                                   
         NI    FILEFLAG,FILE_IS+FILE_REQ+FILE_RCV SET                           
         TM    FILEFLAG,FILE_IS                                                 
         BO    *+8                                                              
         OI    FILEFLAG,FILE_DA    SET DAF                                      
         NC    FILEFLAG,FILTYPE                                                 
         BNZ   EXITOK                                                           
         B     EXITL               SET CC=NE IF FILE DOESNT MATCH TYPE          
         EJECT                                                                  
***********************************************************************         
* TEST IF FILE AVAIL SPACE IS LEQ TO INPUT PERCENTAGE                 *         
***********************************************************************         
TSTPCT   NTR1                                                                   
         OC    FILPCT,FILPCT                                                    
         BZ    EXITOK              NO PCT INPUT SO INCLUDE                      
         CLC   FILPCT,=F'100'                                                   
         BE    EXITOK              INCLUDE IF 100 PERCENT INPUT                 
*                                                                               
         LA    R1,FPCT                                                          
         CLC   FPCT,FIPCT                                                       
         BL    *+8                                                              
         LA    R1,FIPCT                                                         
         TM    0(R1),X'80'                                                      
         BO    EXITL               NO PCT DATA AVAIL SO EXCLUDE                 
         CLC   FILPCT,0(R1)                                                     
         BNL   EXITOK                                                           
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* TEST IF FILE HAS EXTENT ON INPUT SYSNUM OR INPUT CHAN/UNIT          *         
***********************************************************************         
TSTSYS   NTR1                                                                   
         OC    FILSYSNO,FILSYSNO                                                
         BZ    EXITOK              NO VALUE INPUT SO INCLUDE                    
         LA    R5,XNTINFO                                                       
         USING XNTINFOD,R5                                                      
TSTSYS1  CLI   0(R5),X'FF'                                                      
         BE    EXITL               END OF EXTENTS SO EXCLUDE                    
         CLC   FILSYSNO,XSYS                                                    
         BE    EXITOK                                                           
         LA    R5,L'XNTINFO(R5)                                                 
         B     TSTSYS1                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD FILE AND EXTENT INFO FOR FILE DEFINED BY SYSFLES ENTRY AT R1  *         
***********************************************************************         
         USING SYSFLSTD,R1                                                      
GETXTNT  NTR1                                                                   
         XC    FILINFO,FILINFO     EXTRACT BASIC DTF INFO                       
         MVC   FILINFO(8),0(R1)                                                 
         LR    R0,R1               SAVE A(SYSFLES ENTRY)                        
         L     R1,FDTF                                                          
         USING DTFPHD,R1                                                        
         MVC   FNAME,DTFDD                                                      
         MVC   FDEV,DTFDEVT                                                     
         MVC   FPCT,FFS            SET UNDEFINED FIELDS TO HIGH VALUES          
         MVC   FISIZE,FFS                                                       
         MVC   FIAVAIL,FFS                                                      
         MVC   FIPCT,FFS                                                        
         MVC   FLAST,FFS                                                        
         MVC   FINDX,FFS                                                        
         MVC   FOFLO,FFS                                                        
         MVC   FICSIZEU,FFS                                                     
         MVC   FICPCT,FFS                                                       
                                                                                
         TM    DTFTYPE,DTFTBIG     18-BIT                                       
         BZ    *+8                                                              
         OI    FXINF,FX18BIT                                                    
         TM    DTFTYPE,DTFTBIGF    20-BIT                                       
         BZ    *+8                                                              
         OI    FXINF,FX20BIT                                                    
         TM    DTFFLAG,DTFGLOB     GLOBAL TYPE                                  
         BZ    *+8                                                              
         OI    FXINF,FXGLOBAL                                                   
         TM    DTFOPEN,DTF_OPN     IS IT OPEN                                   
         BZ    *+8                                                              
         OI    FXINF,FX_OPEN                                                    
         TM    DTFOPEN,DTF_RO      READ-ONLY                                    
         BZ    *+8                                                              
         OI    FXINF,FX_RO                                                      
         TM    DTFOPEN,DTF_QUI     QUIESCED                                     
         BZ    *+8                                                              
         OI    FXINF,FX_QUI                                                     
         TM    FTYPE,FTYPE_NP      NOP                                          
         BZ    *+8                                                              
         OI    FXINF,FX_NOP                                                     
         NI    FTYPE,255-FTYPE_NP  RE-USE AS DANDX                              
                                                                                
         L     RE,SYSADDR          A(START OF SYSTEM)                           
         USING SYSFLSTD,RE                                                      
         TM    FXINF,FXGLOBAL      GLOBAL?                                      
         BZ    GETX0                                                            
         TM    SYSFSTAT,SYSFSGBL   ON-LINE ONLY GLOBAL                          
         BZ    GETX0                                                            
         OI    FXINF,FXOGLOBL      YES                                          
         DROP  RE                                                               
                                                                                
GETX0    TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BZ    GETX1                                                            
         XC    XNTINFO,XNTINFO     YES, CLEAR DUMMY 1ST EXTENT                  
         NI    FXINF,255-FX22BIT   CLEAR 18/20/22 BIT                           
         MVI   FXNTS+1,1           SET 1 (DUMMY) EXTENT                         
         B     GETXTST             IGNORE FOR NOW BUT CC NEQ                    
*                                                                               
GETX1    TM    FXINF,FX_NOP        File no-oped?                                
         BO    GETXTST             Can't display a no-oped file                 
         XR    RE,RE                                                            
         IC    RE,FDEV             GET TRKS/CYL FOR DEVICE                      
         SLL   RE,3                                                             
         LA    RE,DEVINFO(RE)                                                   
         MVC   FTRKS,4(RE)                                                      
*                                                                               
         USING DTFPHD,R1                                                        
         TM    FTYPE,FTYPE_IS      TEST IS FILE                                 
         BO    GETX3                                                            
         OC    DNEXT,DNEXT                                                      
         BNZ   *+12                                                             
         TM    FTYPE,FTYPE_NF      DID WE HAVE EOF                              
         BO    *+10                NO                                           
         MVC   FLAST,DNEXT                                                      
*                                                                               
         LA    R4,DMTX             R4=A(DA EXTENT MATRIX)                       
         CLC   0(14,R4),=14X'FF'                                                
         BNE   *+8                                                              
         LA    R4,DNDXMTX                                                       
                                                                                
         TM    DIND,DINDNDX                                                     
         BZ    GETX2                                                            
         OI    FTYPE,FTYPE_DX      DANDX TYPE FILE                              
*&&US                                                                           
         CLI   SYS,DEMO_SYS        DEMO SYSTEM?                                 
         BNE   GETX1A                                                           
         CLI   FIL,0               FILE SPECIFIED                               
         BE    GETX1A              NO                                           
         CLC   FIL,HALF+1          MATCH ON THE FILE                            
         BNE   GETX1B                                                           
*&&                                                                             
*                                                                               
GETX1A   XC    DUB,DUB             FILL IN EOF INFO FOR EXTENTS                 
         ST    R1,FULL             SAVE R1                                      
         ST    R1,DMCB+12          A(DTF)                                       
         L     RF,ASYSFACS                                                      
         L     RF,VDADDS-SYSFACD(RF)                                            
         GOTO1 (RF),DMCB,A(FNDEOF),0,0,,DUB                                     
         L     R1,FULL             RESTORE R1                                   
GETX1B   LA    R7,DNEXTMTX         DANDX-STYLE EXTENT MATRIX                    
                                                                                
GETX2    TM    DIND,DINDXAM        HIGH CORE EXTENT MATRIX                      
         BZ    GETX4                                                            
         ICM   R4,15,DMTX                                                       
         BNZ   GETX4                                                            
         B     GETXX               FILE IS NOT OPEN PROBABLY                    
*                                                                               
         USING ISDTF,R1                                                         
GETX3    TM    ISFTYPE,ISFTPDOV                                                 
         BZ    *+8                                                              
         OI    FITFLAG,QISFPDOV    SET NEW STYLE I/S FILE                       
*                                                                               
         TM    ISFTYPE,ISFTBIGF    TEST 20-BIT FILE                             
         BZ    *+8                                                              
         OI    FITFLAG,QISF20B                                                  
*                                                                               
         OC    ISPDLAST,ISPDLAST   EXTRACT IS FILE DATA                         
         BZ    GETX3C                                                           
         TM    ISFTYPE,ISFTPDOV                                                 
         BZ    GETX3A                                                           
         MVC   FLAST,ISOVLAST                                                   
         MVC   FINDX,ISCILAST                                                   
         LA    R4,ISXTNTIX         R4=A(IS EXTENT MATRIX)                       
         B     GETX4                                                            
*                                                                               
GETX3A   MVC   FLAST,ISPDLAST                                                   
         MVC   FINDX,ISCILAST                                                   
         MVC   FOFLO,ISOVLAST                                                   
         SR    RE,RE                                                            
         ICM   RE,3,ISCILN+2                                                    
         ST    RE,FICSIZEU                                                      
GETX3C   SR    RE,RE                                                            
         ICM   RE,3,ISINDXLN                                                    
         TM    ISINDXTY,X'02'      TEST LENGTH IN DBLWDS                        
         BZ    *+8                                                              
         SLL   RE,3                                                             
         ST    RE,FICSIZE                                                       
         MVC   FICADDR,ISINDX                                                   
         MVC   FICFLAG,ISINDXTY                                                 
         LA    R4,ISXTNTIX         R4=A(IS EXTENT MATRIX)                       
*                                                                               
         USING EXTENTD,R4                                                       
GETX4    SAM31                                                                  
         OC    EXTENTD(EXTLNQ),EXTENTD  EXIT IF 1st EXTENT NOT DEFINED          
         BZ    GETXX                                                            
                                                                                
         LA    R5,XNTINFO          R5=A(EXTENT INFO)                            
         USING XNTINFOD,R5                                                      
         SR    R6,R6               R6=EXTENT NUMBER                             
         XC    FULL,FULL           FULL=TOTAL TRACKS SO FAR                     
*                                                                               
GETX5    L     R1,FDTF             EXTRACT INFO FOR THIS EXTENT                 
         USING DTFPHD,R1                                                        
         XC    0(L'XNTINFO,R5),0(R5)                                            
         STC   R6,XNUM                                                          
         MVC   XSYS,EXTCHU                                                      
***********************************************************************         
*SPECIAL CODE TO OVERRIDE MVS CHAN/UNIT                               *         
*DADDS/ISDDS HAS SET CUDV FROM UCB+4(2) WHICH IS LAST CUV USED FOR I/0*         
*NATIVE CHANNEL IS AT UCB+13(3) IN FORMAT C'CHU'                      *         
***********************************************************************         
         L     RE,FDTF             POINT TO DTF                                 
         L     RE,DTFADCB-DTF(RE)  POINT TO DCB                                 
         NILH  GRE,X'00FF'                                                      
         L     RE,44(RE)           POINT TO DEB                                 
         NILH  GRE,X'00FF'                                                      
         LR    RF,R6                                                            
         SLL   RF,4                                                             
         LA    RE,32(RE,RF)        POINT TO DEB MATRIX ENTRY                    
         L     RE,0(RE)            POINT TO UCB FOR XTNT                        
         NILH  GRE,X'00FF'                                                      
         MVC   XSYS,4(RE)                                                       
         L     R1,FDTF                                                          
*                                                                               
         MVC   XLOW,EXTCYLLO       LOW  CCHH                                    
         MVC   XHIGH,EXTCYLHI      HIGH CCHH                                    
         SR    R0,R0               R0=# OF TRACKS FOR EXTENT                    
         ICM   R0,3,EXT#TRKS                                                    
         BZ    GETX5C              EXTENT DATA NOT COMPUTED YET                 
*                                                                               
         TM    FTYPE,FTYPE_IS      EXTRACT DA FILE DATA                         
         BZ    GETX5B                                                           
         TM    FITFLAG,QISF20B     20-BIT IS FILE                               
         BO    GETX5B                                                           
         ST    R0,XMAX             EXTENT CONTAINS HIGH REL TRK                 
         S     R0,FULL             R0=SIZE OF EXTENT                            
         ST    R0,XSIZE                                                         
         B     GETX6                                                            
                                                                                
GETX5B   ST    R0,XSIZE            # OF TRACKS FOR EXTENT                       
         LR    RE,R0                                                            
         A     RE,FULL             FULL IS TOTAL TRACKS                         
         ST    RE,XMAX             SAVE FOR EXTENT RUNNING TOTAL                
         B     GETX6                                                            
                                                                                
GETX5C   LH    RE,XHIGH            COMPUTE EXTENT SIZE                          
         MH    RE,FTRKS                                                         
         AH    RE,XHIGH+2                                                       
         LR    R0,RE                                                            
         LH    RE,XLOW                                                          
         MH    RE,FTRKS                                                         
         AH    RE,XLOW+2                                                        
         SR    R0,RE                                                            
         AHI   R0,1                                                             
         ST    R0,XSIZE                                                         
         LR    RE,R0                                                            
         A     RE,FULL                                                          
         ST    RE,XMAX                                                          
*                                                                               
GETX6    L     RF,FSIZE            ADD EXTENT SIZE TO FILE SIZE                 
         AR    RF,R0                                                            
         ST    RF,FSIZE                                                         
         MVC   XINDEX,FFS                                                       
         MVC   XLAST,FFS                                                        
         CLC   FLAST,FFS           IS LAST RECORD DEFINED                       
         BNE   GETX7A              YES                                          
         MVC   XINUSE,FFS                                                       
         MVC   XAVAIL,FFS                                                       
         MVC   FAVAIL,FFS                                                       
         B     GETX8                                                            
*                                                                               
         USING DTFPHD,R1                                                        
GETX7A   SR    RE,RE               SET RE=TRACK NUMBER OF LAST RECORD           
         SR    RF,RF               SET RF=BLOCK NUMBER OF LAST RECORD           
         TM    FTYPE,FTYPE_DX      DANDX TYPE FILE?                             
         BZ    GETX7A0                                                          
         MVC   FLAST,0(R7)         YES-EACH DNEXT IS THE HIGH TRACK #           
         ICM   RE,3,0(R7)                                                       
         CLI   2(R7),0                                                          
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         ST    RE,XINUSE           WHICH IS EQUIV. TO WHAT'S INUSE              
         MVC   XLAST,FLAST         LAST RECORD IN THIS EXTENT                   
         SR    R0,R0                                                            
         ICM   R0,3,EXT#TRKS                                                    
         B     GETX7E              CALCULATE AVAILABLE TRACKS                   
*                                                                               
GETX7A0  ICM   RE,3,FLAST          16-BIT TRACK                                 
         ICM   RF,1,FLAST+2                                                     
         TM    DTFTYPE,DTFTBIGF+DTFTBIG                                         
         BZ    GETX7B                                                           
         BO    GETX7A2                                                          
         TM    DTFTYPE,DTFTBIGF                                                 
         BO    GETX7A1                                                          
         ICM   RE,7,FLAST          18-BIT TRACK                                 
         SRDL  RE,6                                                             
         SRL   RF,24                                                            
         B     GETX7B                                                           
GETX7A1  ICM   RE,7,FLAST          20-BIT TTTTTB                                
         SRDL  RE,4                                                             
         SRL   RF,28                                                            
         LTR   RF,RF               TEST BLOCKED DISK ADDRESS                    
         BNZ   GETX7B              YES                                          
         IC    RF,FLAST+3          NO RECORD IN LAST BYTE                       
         B     GETX7B                                                           
GETX7A2  ICM   RE,7,FLAST          22-BIT TTTTTX                                
         SRDL  RE,2                                                             
         SRL   RF,30                                                            
         LTR   RF,RF               TEST BLOCKED DISK ADDRESS                    
         BNZ   GETX7B              YES                                          
         IC    RF,FLAST+3          NO RECORD IN LAST BYTE                       
         B     GETX7B                                                           
*                                                                               
GETX7B   C     RE,XMAX             LAST RECORD IN HIGHER EXTENT                 
         BNH   GETX7C                                                           
         XC    XAVAIL,XAVAIL       NO AVAILABLE SPACE                           
         ST    R0,XINUSE                                                        
         B     GETX8                                                            
*                                                                               
GETX7C   C     RE,FULL             LAST RECORD IN LOWER EXTENT                  
         BH    GETX7D                                                           
         XC    XINUSE,XINUSE                                                    
         ST    R0,XAVAIL           ALL SPACE IS AVAILABLE                       
         L     RF,FAVAIL                                                        
         AR    RF,R0                                                            
         ST    RF,FAVAIL                                                        
         B     GETX8                                                            
*                                                                               
GETX7D   MVC   XLAST,FLAST         LAST RECORD IN THIS EXTENT                   
         S     RE,FULL             RE=INUSE COUNT                               
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         BCTR  RE,0                NO DATA RECORDS ON LAST TRACK                
         ST    RE,XINUSE                                                        
*                                                                               
GETX7E   DS    0H                                                               
         SR    R0,RE                                                            
         ST    R0,XAVAIL                                                        
         A     R0,FAVAIL                                                        
         ST    R0,FAVAIL                                                        
*                                                                               
GETX8    MVC   FULL,XMAX           BUMP TO NEXT EXTENT                          
         LA    R6,1(R6)                                                         
         LA    R5,L'XNTINFO(R5)                                                 
         MVI   0(R5),X'FF'                                                      
*                                                                               
         TM    FTYPE,FTYPE_IS      IS IS OR DA?                                 
         BZ    GETX8D              IS ONLY PLEASE SO GO AWAY IF NOT             
         TM    FITFLAG,QISFPDOV    NEW STYLE I/S FILE?                          
         BO    GETX8D              YES-CONTIGUOUS EXTENT MATRIX                 
         CHI   R6,1                CHECK FIRST EXTENT                           
         BH    GETX8D              ALREADY DID                                  
*                                                                               
         USING ISDTF,R1                                                         
         CLI   EXTLNQ(R4),X'FF'    CHECK IF IDX/MST STYLE                       
         BNE   GETX8D                                                           
         L     R1,FDTF             A(DTF)                                       
         LA    R4,ISXTNTPD         NOW SWITCH TO PD EXTENT                      
         B     GETX8F                                                           
         DROP  R1                                                               
*                                                                               
GETX8D   CLI   EXTLNQ(R4),X'FF'    TEST NO MORE EXTENTS                         
         BE    GETXA               NO                                           
         LA    R4,EXTLNQ(R4)                                                    
*                                                                               
GETX8F   TM    FTYPE,FTYPE_DX      DANDX TYPE FILE?                             
         BZ    *+8                                                              
         LA    R7,L'DNEXTMTX(R7)   BUMP TO NEXT DNEXT                           
*                                                                               
         CHI   R6,XNTMAX                                                        
         BL    GETX5                                                            
         DC    H'0'                DIE IF TOO MANY EXTENTS                      
         DROP  R4                                                               
*                                                                               
GETXA    SAM24                                                                  
         STH   R6,FXNTS            SAVE NUMBER OF EXTENTS                       
         XR    RE,RE                                                            
         ICM   RF,15,FIAVAIL       COMPUTE INDEX AVAIL PERCENTAGE               
         BM    GETXA1                                                           
         M     RE,=F'10000'                                                     
         D     RE,FISIZE                                                        
         AHI   RF,50                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         ST    RF,FIPCT                                                         
*                                                                               
GETXA1   L     RF,FAVAIL           COMPUTE FILE AVAIL PERCENTAGE                
         LTR   RF,RF                                                            
         BM    GETXA2                                                           
         M     RE,=F'10000'                                                     
         D     RE,FSIZE                                                         
         AHI   RF,50                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         ST    RF,FPCT                                                          
*                                                                               
GETXA2   L     RE,FICSIZEU         COMPUTE INDEX CORE AVAIL PERCENTAGE          
         LTR   RE,RE                                                            
         BM    GETXA3                                                           
         TM    FICFLAG,X'81'       TEST IF SWITCH TO MASTER FROM CYL            
         BNO   *+14                                                             
         XC    FICPCT,FICPCT       YES THEN NO SPACE                            
         B     GETXA3                                                           
         ICM   RF,15,FICSIZE                                                    
         BZ    GETXA3                                                           
         SR    RF,RE               RF=SIZE-USED                                 
         M     RE,=F'10000'                                                     
         D     RE,FICSIZE                                                       
         AHI   RF,50                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         ST    RF,FICPCT                                                        
GETXA3   DC    0H'0'                                                            
*                                                                               
GETXX    SAM24                                                                  
         CLI   FLXNTS,0            TEST TO SEE IF SET                           
         BE    GETXTST             NO, SO PROCEED AS USUALL                     
         CLC   FXNTS+1(1),HIGHXNT  CHECK XNT OR XNB                             
         BH    GETXNO              DON'T SHOW IF GREATER THAN THIS #            
         CLC   FLXNTS,FXNTS+1                                                   
         BNH   GETXTST             SHOW THIS ONE                                
                                                                                
GETXNO   SR    RE,RE               SET CC TO NOT EQUAL                          
         B     GETXXIT             DON'T SHOW THIS ONE                          
                                                                                
GETXTST  OC    FXNTS,FXNTS         SET CC=EQL IF NO EXTENTS                     
GETXXIT  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DISPLAY EXTENTS AT SCREEN LINE ADDRESSED BY R1                      *         
* FORMAT GIVEN BY FMT FLAGS                                           *         
* X'80'  DISPLAY ONLY FIRST/LAST EXTENTS                              *         
* X'40'  DISPLAY ONLY PART DATA (2-UP)                                *         
* X'20'  DISPLAY ON RIGHT OF SCREEN                                   *         
* X'10'  DISPLAY BIG TOTALS                                           *         
***********************************************************************         
DSPXTNT  NTR1                                                                   
         LR    R8,R1               R8=A(NEXT SCR LINE HDR)                      
         USING FHD,R8                                                           
         XR    R6,R6               R6=NUMBER OF DISPLAYLINES                    
         LA    R5,XNTINFO                                                       
         USING XNTINFOD,R5         R5=A(NEXT XNT DATA)                          
*                                                                               
         LH    R1,FXNTS                                                         
         BCTR  R1,0                                                             
         MHI   R1,L'XNTINFO                                                     
         AR    R1,R5                                                            
         ST    R1,FULL             FULL=A(LAST EXTENT)                          
*                                                                               
         ICM   RF,15,XTNINDEX                                                   
         MHI   RF,L'XNTINFO                                                     
         AR    R5,RF                                                            
*                                                                               
         TM    FMT,FMT_LAST        POSITION TO LAST DA EXTENT                   
         BZ    *+8                                                              
         L     R5,FULL                                                          
*                                                                               
DSPX1    LA    R4,FHDA             POSN TO LEFT SCREEN                          
         TM    FMT,FMT_2UP+FMT_RGHT                                             
         BNO   *+8                                                              
         LA    R4,FHDA+40          POSN TO RIGHT SCREEN                         
         USING XNTDISPD,R4                                                      
*                                                                               
         XC    XDNAME,XDNAME       FILE NAME                                    
         XC    XDPCT,XDPCT         PERCENTAGE AVAIL                             
*                                                                               
         GOTO1 AHEXOUT,PARM,XNUM,XDCHU,1,=C'TOG'                                
*                                                                               
         TM    FMT,FMT_BIG         BIG TOTALS?                                  
         BO    DSPX1D              YES                                          
         MVI   XDCHU+2,C'/'                                                     
         MVC   XDCHU+3(4),=C'VSAM'                                              
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    DSPX1F                                                           
         GOTO1 AHEXOUT,PARM,XSYS,XDCHU+3,L'XSYS,=C'TOG'                         
         B     DSPX1F                                                           
*                                                                               
DSPX1D   DS    0H                                                               
         LLC   R0,XNUM             HIGH EXTENT NUMBER                           
         AHI   R0,1                EXTENT NUMBERS ARE ZERO-BASED                
         EDIT  (R0),XDB#XTNT       TOTAL NUMBER OF EXTENTS                      
*                                                                               
DSPX1F   DS    0H                                                               
         TM    FMT,FMT_BIG         BIG TOTALS?                                  
         BO    DSPX1H              YES                                          
         MVC   XDCCHH(5),DOTS                                                   
         MVI   XDCCHH+5,C'-'                                                    
         MVC   XDCCHH+6(5),DOTS                                                 
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    DSPX1H                                                           
         LH    RF,XLOW             CCCCC-CCCCC                                  
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDCCHH(5),DUB                                                    
         LH    RF,XHIGH                                                         
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDCCHH+6(5),DUB                                                  
*                                                                               
DSPX1H   DS    0H                                                               
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    DSPX1L                                                           
         L     RF,XSIZE            SIZE OF EXTENT                               
         TM    FMT,FMT_LAST                                                     
         BZ    *+16                                                             
         C     R5,FULL                                                          
         BNE   *+8                                                              
         L     RF,FSIZE            SIZE OF FILE IF ONLY LAST EXTENT             
         TM    FMT,FMT_BIG         BIG TOTALS?                                  
         BO    DSPX1J              YES                                          
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDSIZE,DUB                                                       
         B     DSPX1L                                                           
DSPX1J   DS    0H                                                               
         EDIT  (RF),XDBSIZE,COMMAS=YES,ZERO=NOBLANK                             
*                                                                               
DSPX1L   DS    0H                                                               
         TM    FMT,FMT_2UP         TEST FOR FULL DISPLAY                        
         BO    DSPX7               NO                                           
*                                                                               
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    DSPX3                                                            
         MVC   XDINUSE,DOTS                                                     
         MVC   XDAVAIL,DOTS                                                     
         TM    XINUSE,X'80'        INUSE COUNT                                  
         BO    DSPX2                                                            
         L     RF,XINUSE                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         TM    FMT,FMT_BIG         BIG TOTALS?                                  
         BO    *+14                YES                                          
         UNPK  XDINUSE,DUB                                                      
         B     DSPX2                                                            
         L     R0,FSIZE                                                         
         S     R0,FAVAIL                                                        
         EDIT  (R0),XDBINUSE,COMMAS=YES,ZERO=NOBLANK                            
*                                                                               
DSPX2    TM    XAVAIL,X'80'        AVAIL COUNT                                  
         BO    DSPX3                                                            
         L     RF,XAVAIL                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         TM    FMT,FMT_BIG         BIG TOTALS?                                  
         BO    *+14                YES                                          
         UNPK  XDAVAIL,DUB                                                      
         B     DSPX3                                                            
         EDIT  FAVAIL,XDBAVAIL,COMMAS=YES,ZERO=NOBLANK                          
*                                                                               
DSPX3    GOTOR DFILSTAT,XDFILE                                                  
*                                                                               
DSPX4    MVC   XDLAST,DOTS         DISK ADDRESS OF LAST RECORD                  
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BO    DSPX5                                                            
         CLC   XLAST,FFS                                                        
         BE    DSPX5                                                            
         GOTO1 AHEXOUT,PARM,XLAST,XDLAST,4,=C'TOG'                              
*                                                                               
DSPX5    XC    XDDTF,XDDTF                                                      
         XC    XDDEV,XDDEV                                                      
*                                                                               
DSPX7    CHI   R6,1                DISPLAY IS DATA PCT ON SECOND LINE           
         BNL   DSPXB                                                            
*                                                                               
         MVC   XDNAME,FNAME        FIRST LINE DISPLAY                           
         MVC   XDPCT,DOTS                                                       
         TM    FPCT,X'80'                                                       
         BO    DSPX9                                                            
*                                                                               
         L     RF,FPCT                                                          
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XDPCT,DUB                                                        
*                                                                               
DSPX9    DS    0H                                                               
         TM    FMT,FMT_BIG         BIG TOTALS?                                  
         BZ    *+20                NO                                           
         MVC   SRVHL1+15(36),=C'       Total       Inuse       Avail'           
         MVC   SRVHL2+15(36),=C' ----------- ----------- -----------'           
         B     DSPX92                                                           
*                                                                               
         TM    FMT,FMT_2UP         FIRST LINE FULL DATA                         
         BO    DSPXB                                                            
         CLI   FILFMT,X'88'        TEST REQ TO DISPLAY INDEX DATA               
         BNE   DSPX92                                                           
*                                                                               
         LA    R1,SRVHL1                                                        
         MVC   XDLAST-XDDATA(8,R1),=CL8'A(Index)'                               
         MVC   XDDTF-XDDATA(6,R1),=CL6'L''Indx'                                 
         GOTO1 AHEXOUT,PARM,FICADDR,XDLAST,4,=C'TOG'                            
         L     R0,FICSIZE                                                       
         BAS   RE,DSPCVD                                                        
         MVC   XDDTF(6),DUB+2                                                   
         B     DSPX94                                                           
*                                                                               
DSPX92   LA    R0,FDTF                                                          
         AHI   R0,1                                                             
         GOTO1 AHEXOUT,PARM,(R0),XDDTF,3,=C'TOG'                                
*                                                                               
DSPX94   ZIC   RE,FDEV                                                          
         SLL   RE,3                                                             
         LA    RE,DEVINFO(RE)                                                   
         MVC   XDDEV,0(RE)                                                      
*                                                                               
DSPXB    OI    FHOI,FHOITR         BUMP TO NEXT SCREEN LINE                     
         LA    R6,1(R6)            BUMP LINES DISPLAYED                         
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FHLN                                                          
         AR    R8,R1                                                            
*                                                                               
         CLI   FHLN,0              END OF SCREEN?                               
         BNE   DSPXC               NO                                           
*                                                                               
         TM    FMT,FMT_2UP         WANT TWO COLUMN DISPLAY?                     
         BZ    DSPXX               NO                                           
         TM    FMT,FMT_RGHT        ALREADY DID TWO COLUMN DISPLAY?              
         BO    DSPXX               YES                                          
         OI    FMT,FMT_RGHT        DISPLAY ON RIGHT SIDE OF SCREEN              
         LA    R8,SRVDL1H          RESET TO TOP                                 
*                                                                               
DSPXC    C     R5,FULL             EXIT IF LAST EXTENT DISPLAYED                
         BNE   *+14                                                             
         XC    XTNINDEX,XTNINDEX                                                
         B     DSPXX                                                            
*                                                                               
         CLI   FILFMT,X'88'        EXIT IF DISPLAYING INDEX DATA                
         BNE   *+14                                                             
         XC    XTNINDEX,XTNINDEX                                                
         B     DSPXX                                                            
*                                                                               
         LA    RF,SRVLAST                                                       
         CR    R8,RF                                                            
         BE    DSPXX                                                            
*                                                                               
         AHI   R5,L'XNTINFO        BUMP TO NEXT EXTENT                          
         TM    FMT,FMT_LAST                                                     
         BZ    DSPX1                                                            
         TM    FTYPE,FTYPE_IS                                                   
         BZ    DSPXX                                                            
         TM    FITFLAG,QISFPDOV                                                 
         BO    DSPXX                                                            
         L     R5,FULL                                                          
         B     DSPX1                                                            
*                                                                               
DSPXX    LR    R0,R6               RETURN NUMBER OF LINES DISPLAYED             
         LR    R1,R8               RETURN A(NEXT LINE)                          
         XIT1  REGS=(R0,R1)                                                     
         DROP  R4,R8                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION CODE                                                 *         
***********************************************************************         
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
INIT     NTR1                                                                   
         L     R4,SRPARM4                                                       
         USING COMFACSD,R4         GET COMMON FACILITIES                        
         MVC   AHEXIN,CHEXIN                                                    
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ALOCKSPC,CLOCKSPC                                                
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   AUNSCAN,CUNSCAN                                                  
         MVC   AGETTXT,CGETTXT                                                  
         NI    SRVIDH+6,X'BF'                                                   
         XC    MSG,MSG                                                          
         XC    SYSINFO,SYSINFO                                                  
         XC    APQBUFF,APQBUFF                                                  
         XC    PRTUSER,PRTUSER                                                  
         MVI   RW,C' '             SHOW BOTH READONLY AND UPDATIVE              
         CLC   SRVID+1(5),=C'XTNT,'                                             
         BNE   *+10                                                             
         MVC   RW,SRVID+6          =XTNT,+ TO SHOW UPDATIVE SYSTEMS             
*                                                                               
         CLI   SRVP1H+5,0          SET DEFAULTS FOR NO INPUT                    
         BNE   EXIT                                                             
         CLI   SRVP2H+5,0                                                       
         BNE   EXIT                                                             
         CLI   SRVP3H+5,0                                                       
         BNE   EXIT                                                             
         CLI   SRVP4H+5,0                                                       
         BNE   EXIT                                                             
*                                                                               
         MVI   SRVP1H+5,3                                                       
         MVC   SRVP1(3),=C'ALL'    ALL FILES                                    
         MVI   SRVP2H+5,6                                                       
         MVC   SRVP2(6),=C'FMT=P2' PART DISPLAY / 2-UP                          
         MVI   SRVP3H+5,6                                                       
         MVC   SRVP3(6),=C'PCT=20' FILES WITH LESS THAN 20% AVAILABLE           
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILL IN DISPLAY FIELD FOR FILE STATUS                               *         
*                                                                     *         
* 1ST=CLOSED,UPDATE,READ-ONLY,QUEISCED                                *         
* 2ND=GLOBAL                                                          *         
* 3RD=SMALL(16-BIT),MEDIUM (18-BIT),LARGE (20-BIT),G=GIANT(22-BIT)    *         
***********************************************************************         
         PUSH  USING                                                            
         USING XDFILE,R1                                                        
DFILSTAT MVC   XDFILE,SPACES                                                    
         CHI   R6,1                FIRST LINE (R6 SET IN ROUTINES)              
         BNLR  RE                                                               
         MVC   XDFILE,DOTS                                                      
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BZ    *+10                                                             
         MVC   XDFSIZE,=C'VS'      VSAM                                         
         MVI   XDFSTAT,C'N'        NOT OPERATIONAL                              
         TM    FXINF,FX_NOP                                                     
         BOR   RE                                                               
         MVI   XDFSTAT,C'C'        CLOSED                                       
         TM    FXINF,FX_OPEN                                                    
         BZR   RE                                                               
         MVI   XDFSTAT,C'U'        UPDATIVE                                     
         TM    FXINF,FX_RO         READ-ONLY                                    
         BZ    *+8                                                              
         MVI   XDFSTAT,C'R'                                                     
         TM    FXINF,FX_QUI        QUIESED                                      
         BZ    *+8                                                              
         MVI   XDFSTAT,C'Q'                                                     
*                                                                               
         TM    FXINF,FXGLOBAL      DEFINED AS GLOBAL                            
         BZ    *+8                                                              
         MVI   XDFGLOB,C'G'                                                     
         TM    FXINF,FXOGLOBL      SET TO GLOBAL ONLINE ONLY                    
         BZ    *+8                                                              
         MVI   XDFGLOB,C'g'                                                     
*                                                                               
         TM    FFLAGS,FFLG1_VS     VSAM FILE?                                   
         BNZR  RE                                                               
         MVC   XDFSIZE,=C'16'      16-BIT                                       
         TM    FXINF,FX18BIT                                                    
         BZ    *+10                                                             
         MVC   XDFSIZE,=C'18'      18-BIT                                       
         TM    FXINF,FX20BIT                                                    
         BZ    *+10                                                             
         MVC   XDFSIZE,=C'20'      20-BIT                                       
         TM    FXINF,FX18BIT+FX20BIT                                            
         BNO   *+10                                                             
         MVC   XDFSIZE,=C'22'      22-BIT                                       
         BR    RE                                                               
         DROP  R1                                                               
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
                                                                                
DSPCVD   CVD   R0,DUB1             CONVERT R0 TO DECIMAL IN DUB                 
         OI    DUB1+L'DUB1-1,X'0F'                                              
         UNPK  DUB,DUB1                                                         
         BR    RE                                                               
*                                                                               
MAXLINES DC    F'17'                                                            
MAXPCT   DC    F'20'                                                            
DOTS     DC    8C'.'                                                            
FFS      DC    4X'FF'                                                           
XNUMTAB  DC    C'0123456789ABCDEF'                                              
SPACES   DC    132C' '                                                          
*                                                                               
DMREAD   DC    CL8'DMREAD  '                                                    
DMPRINT  DC    CL8'DMPRINT '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
SYSFLES  DC    CL8'SYSFLES '                                                    
PRTQUE   DC    CL8'PRTQUE  '                                                    
*                                                                               
DEVINFO  DS    0CL8                                                             
         DC    C'....',H'12',X'0000'                                            
         DC    C'3340',H'12',X'0000'                                            
         DC    C'3350',H'30',X'0000'                                            
         DC    C'3375',H'12',X'0000'                                            
         DC    C'3380',H'15',X'0000'                                            
         DC    C'3390',H'15',X'0000'                                            
         DC    C'....',H'12',X'0000'                                            
         EJECT                                                                  
***********************************************************************         
* ADDRESSES OF EXCEPTION FILES - SYNONYMS - BY SYSTEM                 *         
***********************************************************************         
SYSTAB   DC    A(SYS00,SYS01,SYS02,SYS03,SYS04,SYS05,SYS06,SYS07)               
         DC    A(SYS08,SYS09,SYS0A,SYS0B,SYS0C,SYS0D,SYS0E,SYS0F)               
         DC    A(SYS10,SYS11,SYS12,SYS13,SYS14,SYS15,SYS16,SYS17)               
         DC    A(SYS18,SYS19,SYS1A,SYS1B,SYS1C,SYS1D,SYS1E,SYS1F)               
         DC    A(SYS20,SYS21,SYS22,SYS23,SYS24,SYS25,SYS26,SYS27)               
         DC    A(SYS28,SYS29,SYS2A,SYS2B,SYS2C,SYS2D,SYS2E,SYS2F)               
         DC    A(SYS30,SYS31,SYS32,SYS33,SYS34,SYS35,SYS36,SYS37)               
         DC    A(SYS38,SYS39,SYS3A,SYS3B,SYS3C,SYS3D,SYS3E,SYS3F)               
         DC    A(SYS40,SYS41,SYS42,SYS43,SYS44,SYS45,SYS46,SYS47)               
         DC    A(SYS48,SYS49,SYS4A,SYS4B,SYS4C,SYS4D,SYS4E,SYS4F)               
         DC    A(SYS50,SYS51,SYS52,SYS53,SYS54,SYS55,SYS56,SYS57)               
         DC    A(SYS58,SYS59,SYS5A,SYS5B,SYS5C,SYS5D,SYS5E,SYS5F)               
         DC    A(SYS60,SYS61,SYS62,SYS63,SYS64,SYS65,SYS66,SYS67)               
         DC    A(SYS68,SYS69,SYS6A,SYS6B,SYS6C,SYS6D,SYS6E,SYS6F)               
         DC    A(SYS70,SYS71,SYS72,SYS73,SYS74,SYS75,SYS76,SYS77)               
         DC    A(SYS78,SYS79,SYS7A,SYS7B,SYS7C,SYS7D,SYS7E,SYS7F)               
         DC    A(SYS80,SYS81,SYS82,SYS83,SYS84,SYS85,SYS86,SYS87)               
         DC    A(SYS88,SYS89,SYS8A,SYS8B,SYS8C,SYS8D,SYS8E,SYS8F)               
         DC    A(SYS90,SYS91,SYS92,SYS93,SYS94,SYS95,SYS96,SYS97)               
         DC    A(SYS98,SYS99,SYS9A,SYS9B,SYS9C,SYS9D,SYS9E,SYS9F)               
         DC    A(SYSA0,SYSA1,SYSA2,SYSA3,SYSA4,SYSA5,SYSA6,SYSA7)               
         DC    A(SYSA8,SYSA9,SYSAA,SYSAB,SYSAC,SYSAD,SYSAE,SYSAF)               
         DC    A(SYSB0,SYSB1,SYSB2,SYSB3,SYSB4,SYSB5,SYSB6,SYSB7)               
         DC    A(SYSB8,SYSB9,SYSBA,SYSBB,SYSBC,SYSBD,SYSBE,SYSBF)               
         DC    A(SYSC0,SYSC1,SYSC2,SYSC3,SYSC4,SYSC5,SYSC6,SYSC7)               
         DC    A(SYSC8,SYSC9,SYSCA,SYSCB,SYSCC,SYSCD,SYSCE,SYSCF)               
         DC    A(SYSD0,SYSD1,SYSD2,SYSD3,SYSD4,SYSD5,SYSD6,SYSD7)               
         DC    A(SYSD8,SYSD9,SYSDA,SYSDB,SYSDC,SYSDD,SYSDE,SYSDF)               
         DC    A(SYSE0,SYSE1,SYSE2,SYSE3,SYSE4,SYSE5,SYSE6,SYSE7)               
         DC    A(SYSE8,SYSE9,SYSEA,SYSEB,SYSEC,SYSED,SYSEE,SYSEF)               
         DC    A(SYSF0,SYSF1,SYSF2,SYSF3,SYSF4,SYSF5,SYSF6,SYSF7)               
         DC    A(SYSF8,SYSF9,SYSFA,SYSFB,SYSFC,SYSFD,SYSFE,SYSFF)               
*                                                                               
SYSSYN   DS    0CL2                TABLE OF SYS/FIL SYNONYMS                    
***********************************************************************         
* US SYNONYMS                                                         *         
***********************************************************************         
*&&DO                                                                           
         DC    X'07A107AE07AF070607080709070B070C070D070E070F' SPT3             
         DC    X'0DA10DAE0DAF0D060D080D090D0B0D0C0D0D0D0E0D0F' SPT4             
         DC    X'11A111AE11AF110611081109110B110C110D110E110F' SPT5             
         DC    X'15A115AE15AF150615081509150B150C150D150E150F' SPT6             
         DC    X'22A122AE22AF220622082209220B220C220D220E220F' SPT7             
         DC    X'23A123AE23AF230623082309230B230C230D230E230F' SPT8=N1          
         DC    X'25A125AE25AF250625082509250B250C250D250E250F' SPT9=N2          
         DC    X'26A126AE26AF260626082609260B260C260D260E260F' SPTA=N3          
         DC    X'27A127AE27AF270627082709270B270C270D270E270F' SPTB             
         DC    X'28A128AE28AF280628082809280B280C280D280E280F' SPTC=N4          
         DC    X'E2A1E2AEE2AFE206E208E209E20BE20CE20DE20EE20F' SPTE             
         DC    X'F2A1F2AEF2AFF206F208F209F20BF20CF20DF20EF20F' SPTF             
         DC    X'F3A1F3AEF3AFF306F308F309F30BF30CF30DF30EF30F' SPTG             
         DC    X'F4A1F4AEF4AFF406F408F409F40BF40CF40DF40EF40F' SPTH             
         DC    X'29A129AE29AF290629082909290B290C290D290E290F' SPTJ=N5          
         DC    X'2AA12AAE2AAF2A062A082A092A0B2A0C2A0D2A0E2A0F' SPTK=N6          
         DC    X'F5A1F5AEF5AFF506F508F509F50BF50CF50DF50EF50F' SPTL             
         DC    X'F6A1F6AEF6AFF606F608F609F60BF60CF60DF60EF60F' SPTM             
         DC    X'F7A1F7AEF7AFF706F708F709F70BF70CF70DF70EF70F' SPTM             
         DC    X'D2A1D2AED2AFD206D208D209D20BD20CD20DD20ED20F' SPTP=N7          
         DC    X'32A132AE32AF320632083209320B320C320D320E320F' SPTQ             
         DC    X'33A133AE33AF330633083309330B330C330D330E330F' SPTS             
         DC    X'2EA12EAE2EAF2E062E082E092E0B2E0C2E0D2E0E2E0F' SPTT=NT          
         DC    X'36A136AE36AF360636083609360B360C360D360E360F' SPTU             
         DC    X'F8A1F8AEF8AFF806F808F809F80BF80CF80DF80EF80F' SPTV             
         DC    X'2BA12BAE2BAF2B062B082B092B0B2B0C2B0D2B0E2B0F' SPTW=NW          
         DC    X'F9A1F9AEF9AFF906F908F909F90BF90CF90DF90EF90F' SPTY             
         DC    X'FAA1FAAEFAAFFA06FA08FA09FA0BFA0CFA0DFA0EFA0F' SPT0             
*&&                                                                             
*                                                                               
         DC    X'A1AEAF26282C2D2E2F303138393B3D3E3F4849'                        
*&&US                                                                           
SYS00    DC    X'FF'                                                            
SYS01    DC    X'A1AEAFFF'                    SERVICE                           
SYS02    DC    X'A1AEAFFF'                    SPT1                              
SYS03    DC    X'A1AEAF26282C2D2E2F303138393B3D3E3F4849'  SPT2                  
SYS04    DC    X'A1AEAFFF'                    PRNT1                             
SYS05    DC    X'FF'                                                            
SYS06    DC    X'A1AEAFFF'                    ACC1                              
SYS07    DC    X'A1AEAF26282C2D2E2F303138393B3D3E3F4849'  SPT3                  
SYS08    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP1                              
SYS09    DC    X'A1AEAFFF'                    MBA1                              
SYS0A    DC    X'FF'                                                            
SYS0B    DC    X'FF'                                                            
SYS0C    DC    X'A1AEAFFF'                    DEMO                              
SYS0D    DC    X'FF'                                                            
SYS0E    DC    X'A1AEAFFF'                    PER1                              
SYS0F    DC    X'FF'                                                            
*                                                                               
SYS10    DC    X'A1AEAFFF'                    TAL1                              
SYS11    DC    X'FF'                                                            
SYS12    DC    X'FF'                                                            
SYS13    DC    X'FF'                                                            
SYS14    DC    X'A1AEAFFF'                    PRNT2                             
SYS15    DC    X'FF'                                                            
SYS16    DC    X'A1AEAFFF'                    ACC2                              
SYS17    DC    X'A1AEAFFF'                    ACC3                              
SYS18    DC    X'A1AEAFFF'                    ACC4                              
SYS19    DC    X'A1AEAFFF'                    ACC5                              
SYS1A    DC    X'A1AEAFFF'                    ACCY                              
SYS1B    DC    X'A1AEAFFF'                    ACCD                              
SYS1C    DC    X'FF'                                                            
SYS1D    DC    X'FF'                                                            
SYS1E    DC    X'A1AEAFFF'                    PER2                              
SYS1F    DC    X'FF'                                                            
*                                                                               
SYS20    DC    X'A1AEAFFF'                    TAL2                              
SYS21    DC    X'FF'                                                            
SYS22    DC    X'FF'                                                            
SYS23    DC    X'FF'                                                            
SYS24    DC    X'A1AEAFFF'                    PRNT3                             
SYS25    DC    X'FF'                                                            
SYS26    DC    X'FF'                                                            
SYS27    DC    X'FF'                                                            
SYS28    DC    X'FF'                                                            
SYS29    DC    X'FF'                                                            
SYS2A    DC    X'FF'                                                            
SYS2B    DC    X'FF'                                                            
SYS2C    DC    X'FF'                                                            
SYS2D    DC    X'FF'                                                            
SYS2E    DC    X'FF'                                                            
SYS2F    DC    X'FF'                                                            
*                                                                               
SYS30    DC    X'A1AEAFFF'                    TAL3                              
SYS31    DC    X'FF'                                                            
SYS32    DC    X'FF'                                                            
SYS33    DC    X'FF'                                                            
SYS34    DC    X'A1AEAFFF'                    PRNT4                             
SYS35    DC    X'FF'                                                            
SYS36    DC    X'FF'                                                            
SYS37    DC    X'A1AEAFFF'                    ACCV                              
SYS38    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP2                              
SYS39    DC    X'FF'                                                            
SYS3A    DC    X'FF'                                                            
SYS3B    DC    X'FF'                                                            
SYS3C    DC    X'FF'                                                            
SYS3D    DC    X'FF'                                                            
SYS3E    DC    X'FF'                                                            
SYS3F    DC    X'FF'                                                            
*                                                                               
SYS40    DC    X'FF'                                                            
SYS41    DC    X'A1AEAFFF'                    TRF1                              
SYS42    DC    X'A1AEAFFF'                    TRF2                              
SYS43    DC    X'A1AEAFFF'                    TRF3                              
SYS44    DC    X'A1AEAFFF'                    PRNT5                             
SYS45    DC    X'FF'                                                            
SYS46    DC    X'A1AEAFFF'                    TRF4                              
SYS47    DC    X'A1AEAFFF'                    TRF5                              
SYS48    DC    X'A1AEAFFF'                    TRF6                              
SYS49    DC    X'A1AEAFFF'                    TRF7                              
SYS4A    DC    X'A1AEAFFF'                    TRFB                              
SYS4B    DC    X'A1AEAFFF'                    TRFE                              
SYS4C    DC    X'A1AEAFFF'                    TRFF                              
SYS4D    DC    X'A1AEAFFF'                    TRFG                              
SYS4E    DC    X'A1AEAFFF'                    TRFH                              
SYS4F    DC    X'FF'                                                            
*                                                                               
SYS50    DC    X'FF'                                                            
SYS51    DC    X'A1AEAFFF'                    TRFL                              
SYS52    DC    X'A1AEAFFF'                    TRFM                              
SYS53    DC    X'A1AEAFFF'                    TRFN                              
SYS54    DC    X'A1AEAFFF'                    PRNT6                             
SYS55    DC    X'FF'                                                            
SYS56    DC    X'A1AEAFFF'                    TRFQ                              
SYS57    DC    X'A1AEAFFF'                    TRFS                              
SYS58    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP3                              
SYS59    DC    X'A1AEAFFF'                    TRFU                              
SYS5A    DC    X'A1AEAFFF'                    TRFV                              
SYS5B    DC    X'A1AEAFFF'                    TRFY                              
SYS5C    DC    X'A1AEAFFF'                    TRF0                              
SYS5D    DC    X'FF'                                                            
SYS5E    DC    X'FF'                                                            
SYS5F    DC    X'FF'                                                            
*                                                                               
SYS60    DC    X'FF'                                                            
SYS61    DC    X'FF'                                                            
SYS62    DC    X'FF'                                                            
SYS63    DC    X'FF'                                                            
SYS64    DC    X'A1AEAFFF'                    PRNT7                             
SYS65    DC    X'FF'                                                            
SYS66    DC    X'A1AEAFFF'                    ACC6                              
SYS67    DC    X'FF'                                                            
SYS68    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP4                              
SYS69    DC    X'FF'                                                            
SYS6A    DC    X'FF'                                                            
SYS6B    DC    X'FF'                                                            
SYS6C    DC    X'FF'                                                            
SYS6D    DC    X'FF'                                                            
SYS6E    DC    X'FF'                                                            
SYS6F    DC    X'FF'                                                            
*                                                                               
SYS70    DC    X'FF'                                                            
SYS71    DC    X'FF'                                                            
SYS72    DC    X'FF'                                                            
SYS73    DC    X'FF'                                                            
SYS74    DC    X'A1AEAFFF'                    PRNT8                             
SYS75    DC    X'FF'                                                            
SYS76    DC    X'A1AEAFFF'                    ACC7                              
SYS77    DC    X'FF'                                                            
SYS78    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP5                              
SYS79    DC    X'FF'                                                            
SYS7A    DC    X'FF'                                                            
SYS7B    DC    X'FF'                                                            
SYS7C    DC    X'FF'                                                            
SYS7D    DC    X'FF'                                                            
SYS7E    DC    X'FF'                                                            
SYS7F    DC    X'FF'                                                            
*                                                                               
SYS80    DC    X'FF'                                                            
SYS81    DC    X'FF'                                                            
SYS82    DC    X'FF'                                                            
SYS83    DC    X'FF'                                                            
SYS84    DC    X'A1AEAFFF'                    PRNT9                             
SYS85    DC    X'FF'                                                            
SYS86    DC    X'A1AEAFFF'                    ACC8                              
SYS87    DC    X'FF'                                                            
SYS88    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP6                              
SYS89    DC    X'FF'                                                            
SYS8A    DC    X'FF'                                                            
SYS8B    DC    X'FF'                                                            
SYS8C    DC    X'FF'                                                            
SYS8D    DC    X'FF'                                                            
SYS8E    DC    X'FF'                                                            
SYS8F    DC    X'FF'                                                            
*                                                                               
SYS90    DC    X'FF'                                                            
SYS91    DC    X'FF'                                                            
SYS92    DC    X'FF'                                                            
SYS93    DC    X'FF'                                                            
SYS94    DC    X'FF'                                                            
SYS95    DC    X'FF'                                                            
SYS96    DC    X'A1AEAFFF'                    ACC9                              
SYS97    DC    X'FF'                                                            
SYS98    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP7                              
SYS99    DC    X'FF'                                                            
SYS9A    DC    X'FF'                                                            
SYS9B    DC    X'FF'                                                            
SYS9C    DC    X'FF'                                                            
SYS9D    DC    X'FF'                                                            
SYS9E    DC    X'FF'                                                            
SYS9F    DC    X'FF'                                                            
*                                                                               
SYSA0    DC    X'FF'                                                            
SYSA1    DC    X'FF'                                                            
SYSA2    DC    X'FF'                                                            
SYSA3    DC    X'FF'                                                            
SYSA4    DC    X'FF'                                                            
SYSA5    DC    X'FF'                                                            
SYSA6    DC    X'A1AEAFFF'                    ACCA                              
SYSA7    DC    X'FF'                                                            
SYSA8    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP8                              
SYSA9    DC    X'FF'                                                            
SYSAA    DC    X'FF'                                                            
SYSAB    DC    X'FF'                                                            
SYSAC    DC    X'FF'                                                            
SYSAD    DC    X'FF'                                                            
SYSAE    DC    X'FF'                                                            
SYSAF    DC    X'FF'                                                            
*                                                                               
SYSB0    DC    X'FF'                                                            
SYSB1    DC    X'FF'                                                            
SYSB2    DC    X'FF'                                                            
SYSB3    DC    X'FF'                                                            
SYSB4    DC    X'FF'                                                            
SYSB5    DC    X'FF'                                                            
SYSB6    DC    X'A1AEAFFF'                    ACCB                              
SYSB7    DC    X'FF'                                                            
SYSB8    DC    X'A1AEAF0608090B0C0D0E0FFF'    REP9                              
SYSB9    DC    X'FF'                                                            
SYSBA    DC    X'FF'                                                            
SYSBB    DC    X'FF'                                                            
SYSBC    DC    X'FF'                                                            
SYSBD    DC    X'FF'                                                            
SYSBE    DC    X'FF'                                                            
SYSBF    DC    X'FF'                                                            
*                                                                               
SYSC0    DC    X'FF'                                                            
SYSC1    DC    X'FF'                                                            
SYSC2    DC    X'FF'                                                            
SYSC3    DC    X'FF'                                                            
SYSC4    DC    X'FF'                                                            
SYSC5    DC    X'FF'                                                            
SYSC6    DC    X'A1AEAFFF'                    ACCQ                              
SYSC7    DC    X'FF'                                                            
SYSC8    DC    X'A1AEAF0608090B0C0D0E0FFF'    REPY                              
SYSC9    DC    X'FF'                                                            
SYSCA    DC    X'FF'                                                            
SYSCB    DC    X'FF'                                                            
SYSCC    DC    X'FF'                                                            
SYSCD    DC    X'FF'                                                            
SYSCE    DC    X'FF'                                                            
SYSCF    DC    X'FF'                                                            
*                                                                               
SYSD0    DC    X'FF'                                                            
SYSD1    DC    X'FF'                                                            
SYSD2    DC    X'FF'                                                            
SYSD3    DC    X'FF'                                                            
SYSD4    DC    X'FF'                                                            
SYSD5    DC    X'FF'                                                            
SYSD6    DC    X'A1AEAFFF'                    ACCC                              
SYSD7    DC    X'FF'                                                            
SYSD8    DC    X'FF'                                                            
SYSD9    DC    X'FF'                                                            
SYSDA    DC    X'FF'                                                            
SYSDB    DC    X'FF'                                                            
SYSDC    DC    X'FF'                                                            
SYSDD    DC    X'FF'                                                            
SYSDE    DC    X'FF'                                                            
SYSDF    DC    X'FF'                                                            
*                                                                               
SYSE0    DC    X'FF'                                                            
SYSE1    DC    X'FF'                                                            
SYSE2    DC    X'FF'                                                            
SYSE3    DC    X'FF'                                                            
SYSE4    DC    X'FF'                                                            
SYSE5    DC    X'FF'                                                            
SYSE6    DC    X'A1AEAFFF'                    ACCT                              
SYSE7    DC    X'FF'                                                            
SYSE8    DC    X'FF'                                                            
SYSE9    DC    X'FF'                                                            
SYSEA    DC    X'FF'                                                            
SYSEB    DC    X'FF'                                                            
SYSEC    DC    X'FF'                                                            
SYSED    DC    X'FF'                                                            
SYSEE    DC    X'FF'                                                            
SYSEF    DC    X'FF'                                                            
*                                                                               
SYSF0    DC    X'FF'                                                            
SYSF1    DC    X'FF'                                                            
SYSF2    DC    X'FF'                                                            
SYSF3    DC    X'FF'                                                            
SYSF4    DC    X'FF'                                                            
SYSF5    DC    X'FF'                                                            
SYSF6    DC    X'FF'                                                            
SYSF7    DC    X'FF'                                                            
SYSF8    DC    X'FF'                                                            
SYSF9    DC    X'FF'                                                            
SYSFA    DC    X'FF'                                                            
SYSFB    DC    X'FF'                                                            
SYSFC    DC    X'FF'                                                            
SYSFD    DC    X'FF'                                                            
SYSFE    DC    X'FF'                                                            
SYSFF    DC    X'FF'                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* UK SYNONYMS                                                         *         
***********************************************************************         
*&&UK                                                                           
SYS00    DC    X'A1AEAFFF'                                                      
SYS01    DC    X'A1AEAFFF'                                                      
SYS02    DC    X'A1AEAFFF'                                                      
SYS03    DC    X'A1AEAFFF'                                                      
SYS04    DC    X'A1AEAFFF'                                                      
SYS05    DC    X'A1AEAFFF'                                                      
SYS06    DC    X'A1AEAFFF'                                                      
SYS07    DC    X'A1AEAFFF'                                                      
SYS08    DC    X'A1AEAFFF'                                                      
SYS09    DC    X'A1AEAFFF'                                                      
SYS0A    DC    X'FF'                                                            
SYS0B    DC    X'A1AEAFFF'                                                      
SYS0C    DC    X'A1AEAFFF'                                                      
SYS0D    DC    X'A1AEAFFF'                                                      
SYS0E    DC    X'A1AEAFFF'                                                      
SYS0F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS10    DC    X'A1AEAFFF'                                                      
SYS11    DC    X'A1AEAFFF'                                                      
SYS12    DC    X'A1AEAFFF'                                                      
SYS13    DC    X'A1AEAFFF'                                                      
SYS14    DC    X'A1AEAFFF'                                                      
SYS15    DC    X'A1AEAFFF'                                                      
SYS16    DC    X'A1AEAFFF'                                                      
SYS17    DC    X'A1AEAFFF'                                                      
SYS18    DC    X'A1AEAFFF'                                                      
SYS19    DC    X'A1AEAFFF'                                                      
SYS1A    DC    X'A1AEAFFF'                                                      
SYS1B    DC    X'A1AEAFFF'                                                      
SYS1C    DC    X'A1AEAFFF'                                                      
SYS1D    DC    X'A1AEAFFF'                                                      
SYS1E    DC    X'A1AEAFFF'                                                      
SYS1F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS20    DC    X'A1AEAFFF'                                                      
SYS21    DC    X'A1AEAFFF'                                                      
SYS22    DC    X'A1AEAFFF'                                                      
SYS23    DC    X'A1AEAFFF'                                                      
SYS24    DC    X'A1AEAFFF'                                                      
SYS25    DC    X'A1AEAFFF'                                                      
SYS26    DC    X'A1AEAFFF'                                                      
SYS27    DC    X'A1AEAFFF'                                                      
SYS28    DC    X'A1AEAFFF'                                                      
SYS29    DC    X'A1AEAFFF'                                                      
SYS2A    DC    X'A1AEAFFF'                                                      
SYS2B    DC    X'A1AEAFFF'                                                      
SYS2C    DC    X'A1AEAFFF'                                                      
SYS2D    DC    X'A1AEAFFF'                                                      
SYS2E    DC    X'A1AEAFFF'                                                      
SYS2F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS30    DC    X'A1AEAFFF'                                                      
SYS31    DC    X'A1AEAFFF'                                                      
SYS32    DC    X'A1AEAFFF'                                                      
SYS33    DC    X'A1AEAFFF'                                                      
SYS34    DC    X'A1AEAFFF'                                                      
SYS35    DC    X'A1AEAFFF'                                                      
SYS36    DC    X'A1AEAFFF'                                                      
SYS37    DC    X'A1AEAFFF'                                                      
SYS38    DC    X'A1AEAFFF'                                                      
SYS39    DC    X'A1AEAFFF'                                                      
SYS3A    DC    X'A1AEAFFF'                                                      
SYS3B    DC    X'A1AEAFFF'                                                      
SYS3C    DC    X'A1AEAFFF'                                                      
SYS3D    DC    X'A1AEAFFF'                                                      
SYS3E    DC    X'A1AEAFFF'                                                      
SYS3F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS40    DC    X'A1AEAFFF'                                                      
SYS41    DC    X'A1AEAFFF'                                                      
SYS42    DC    X'A1AEAFFF'                                                      
SYS43    DC    X'A1AEAFFF'                                                      
SYS44    DC    X'A1AEAFFF'                                                      
SYS45    DC    X'A1AEAFFF'                                                      
SYS46    DC    X'A1AEAFFF'                                                      
SYS47    DC    X'A1AEAFFF'                                                      
SYS48    DC    X'A1AEAFFF'                                                      
SYS49    DC    X'A1AEAFFF'                                                      
SYS4A    DC    X'A1AEAFFF'                                                      
SYS4B    DC    X'A1AEAFFF'                                                      
SYS4C    DC    X'A1AEAFFF'                                                      
SYS4D    DC    X'A1AEAFFF'                                                      
SYS4E    DC    X'A1AEAFFF'                                                      
SYS4F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS50    DC    X'A1AEAFFF'                                                      
SYS51    DC    X'A1AEAFFF'                                                      
SYS52    DC    X'A1AEAFFF'                                                      
SYS53    DC    X'A1AEAFFF'                                                      
SYS54    DC    X'A1AEAFFF'                                                      
SYS55    DC    X'A1AEAFFF'                                                      
SYS56    DC    X'A1AEAFFF'                                                      
SYS57    DC    X'A1AEAFFF'                                                      
SYS58    DC    X'A1AEAFFF'                                                      
SYS59    DC    X'A1AEAFFF'                                                      
SYS5A    DC    X'A1AEAFFF'                                                      
SYS5B    DC    X'A1AEAFFF'                                                      
SYS5C    DC    X'A1AEAFFF'                                                      
SYS5D    DC    X'A1AEAFFF'                                                      
SYS5E    DC    X'A1AEAFFF'                                                      
SYS5F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS60    DC    X'A1AEAFFF'                                                      
SYS61    DC    X'A1AEAFFF'                                                      
SYS62    DC    X'A1AEAFFF'                                                      
SYS63    DC    X'A1AEAFFF'                                                      
SYS64    DC    X'A1AEAFFF'                                                      
SYS65    DC    X'A1AEAFFF'                                                      
SYS66    DC    X'A1AEAFFF'                                                      
SYS67    DC    X'A1AEAFFF'                                                      
SYS68    DC    X'A1AEAFFF'                                                      
SYS69    DC    X'A1AEAFFF'                                                      
SYS6A    DC    X'A1AEAFFF'                                                      
SYS6B    DC    X'A1AEAFFF'                                                      
SYS6C    DC    X'A1AEAFFF'                                                      
SYS6D    DC    X'A1AEAFFF'                                                      
SYS6E    DC    X'A1AEAFFF'                                                      
SYS6F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS70    DC    X'A1AEAFFF'                                                      
SYS71    DC    X'A1AEAFFF'                                                      
SYS72    DC    X'A1AEAFFF'                                                      
SYS73    DC    X'A1AEAFFF'                                                      
SYS74    DC    X'A1AEAFFF'                                                      
SYS75    DC    X'A1AEAFFF'                                                      
SYS76    DC    X'A1AEAFFF'                                                      
SYS77    DC    X'A1AEAFFF'                                                      
SYS78    DC    X'A1AEAFFF'                                                      
SYS79    DC    X'A1AEAFFF'                                                      
SYS7A    DC    X'A1AEAFFF'                                                      
SYS7B    DC    X'A1AEAFFF'                                                      
SYS7C    DC    X'A1AEAFFF'                                                      
SYS7D    DC    X'A1AEAFFF'                                                      
SYS7E    DC    X'A1AEAFFF'                                                      
SYS7F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS80    DC    X'A1AEAFFF'                                                      
SYS81    DC    X'A1AEAFFF'                                                      
SYS82    DC    X'A1AEAFFF'                                                      
SYS83    DC    X'A1AEAFFF'                                                      
SYS84    DC    X'A1AEAFFF'                                                      
SYS85    DC    X'A1AEAFFF'                                                      
SYS86    DC    X'A1AEAFFF'                                                      
SYS87    DC    X'A1AEAFFF'                                                      
SYS88    DC    X'A1AEAFFF'                                                      
SYS89    DC    X'A1AEAFFF'                                                      
SYS8A    DC    X'A1AEAFFF'                                                      
SYS8B    DC    X'A1AEAFFF'                                                      
SYS8C    DC    X'A1AEAFFF'                                                      
SYS8D    DC    X'A1AEAFFF'                                                      
SYS8E    DC    X'A1AEAFFF'                                                      
SYS8F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYS90    DC    X'A1AEAFFF'                                                      
SYS91    DC    X'A1AEAFFF'                                                      
SYS92    DC    X'A1AEAFFF'                                                      
SYS93    DC    X'A1AEAFFF'                                                      
SYS94    DC    X'A1AEAFFF'                                                      
SYS95    DC    X'A1AEAFFF'                                                      
SYS96    DC    X'A1AEAFFF'                                                      
SYS97    DC    X'A1AEAFFF'                                                      
SYS98    DC    X'A1AEAFFF'                                                      
SYS99    DC    X'A1AEAFFF'                                                      
SYS9A    DC    X'A1AEAFFF'                                                      
SYS9B    DC    X'A1AEAFFF'                                                      
SYS9C    DC    X'A1AEAFFF'                                                      
SYS9D    DC    X'A1AEAFFF'                                                      
SYS9E    DC    X'A1AEAFFF'                                                      
SYS9F    DC    X'A1AEAFFF'                                                      
*                                                                               
SYSA0    DC    X'A1AEAFFF'                                                      
SYSA1    DC    X'A1AEAFFF'                                                      
SYSA2    DC    X'A1AEAFFF'                                                      
SYSA3    DC    X'A1AEAFFF'                                                      
SYSA4    DC    X'A1AEAFFF'                                                      
SYSA5    DC    X'A1AEAFFF'                                                      
SYSA6    DC    X'A1AEAFFF'                                                      
SYSA7    DC    X'A1AEAFFF'                                                      
SYSA8    DC    X'A1AEAFFF'                                                      
SYSA9    DC    X'A1AEAFFF'                                                      
SYSAA    DC    X'A1AEAFFF'                                                      
SYSAB    DC    X'A1AEAFFF'                                                      
SYSAC    DC    X'A1AEAFFF'                                                      
SYSAD    DC    X'A1AEAFFF'                                                      
SYSAE    DC    X'A1AEAFFF'                                                      
SYSAF    DC    X'A1AEAFFF'                                                      
*                                                                               
SYSB0    DC    X'A1AEAFFF'                                                      
SYSB1    DC    X'A1AEAFFF'                                                      
SYSB2    DC    X'A1AEAFFF'                                                      
SYSB3    DC    X'A1AEAFFF'                                                      
SYSB4    DC    X'A1AEAFFF'                                                      
SYSB5    DC    X'A1AEAFFF'                                                      
SYSB6    DC    X'A1AEAFFF'                                                      
SYSB7    DC    X'A1AEAFFF'                                                      
SYSB8    DC    X'A1AEAFFF'                                                      
SYSB9    DC    X'A1AEAFFF'                                                      
SYSBA    DC    X'A1AEAFFF'                                                      
SYSBB    DC    X'A1AEAFFF'                                                      
SYSBC    DC    X'A1AEAFFF'                                                      
SYSBD    DC    X'A1AEAFFF'                                                      
SYSBE    DC    X'A1AEAFFF'                                                      
SYSBF    DC    X'A1AEAFFF'                                                      
*                                                                               
SYSC0    DC    X'A1AEAFFF'                                                      
SYSC1    DC    X'A1AEAFFF'                                                      
SYSC2    DC    X'A1AEAFFF'                                                      
SYSC3    DC    X'A1AEAFFF'                                                      
SYSC4    DC    X'A1AEAFFF'                                                      
SYSC5    DC    X'A1AEAFFF'                                                      
SYSC6    DC    X'A1AEAFFF'                                                      
SYSC7    DC    X'A1AEAFFF'                                                      
SYSC8    DC    X'A1AEAFFF'                                                      
SYSC9    DC    X'A1AEAFFF'                                                      
SYSCA    DC    X'A1AEAFFF'                                                      
SYSCB    DC    X'A1AEAFFF'                                                      
SYSCC    DC    X'A1AEAFFF'                                                      
SYSCD    DC    X'A1AEAFFF'                                                      
SYSCE    DC    X'A1AEAFFF'                                                      
SYSCF    DC    X'A1AEAFFF'                                                      
*                                                                               
SYSD0    DC    X'A1AEAFFF'                                                      
SYSD1    DC    X'A1AEAFFF'                                                      
SYSD2    DC    X'A1AEAFFF'                                                      
SYSD3    DC    X'A1AEAFFF'                                                      
SYSD4    DC    X'A1AEAFFF'                                                      
SYSD5    DC    X'A1AEAFFF'                                                      
SYSD6    DC    X'A1AEAFFF'                                                      
SYSD7    DC    X'A1AEAFFF'                                                      
SYSD8    DC    X'A1AEAFFF'                                                      
SYSD9    DC    X'A1AEAFFF'                                                      
SYSDA    DC    X'A1AEAFFF'                                                      
SYSDB    DC    X'A1AEAFFF'                                                      
SYSDC    DC    X'A1AEAFFF'                                                      
SYSDD    DC    X'A1AEAFFF'                                                      
SYSDE    DC    X'A1AEAFFF'                                                      
SYSDF    DC    X'A1AEAFFF'                                                      
*                                                                               
SYSE0    DC    X'A1AEAFFF'                                                      
SYSE1    DC    X'A1AEAFFF'                                                      
SYSE2    DC    X'A1AEAFFF'                                                      
SYSE3    DC    X'A1AEAFFF'                                                      
SYSE4    DC    X'A1AEAFFF'                                                      
SYSE5    DC    X'A1AEAFFF'                                                      
SYSE6    DC    X'A1AEAFFF'                                                      
SYSE7    DC    X'A1AEAFFF'                                                      
SYSE8    DC    X'A1AEAFFF'                                                      
SYSE9    DC    X'A1AEAFFF'                                                      
SYSEA    DC    X'A1AEAFFF'                                                      
SYSEB    DC    X'A1AEAFFF'                                                      
SYSEC    DC    X'A1AEAFFF'                                                      
SYSED    DC    X'A1AEAFFF'                                                      
SYSEE    DC    X'A1AEAFFF'                                                      
SYSEF    DC    X'A1AEAFFF'                                                      
*                                                                               
SYSF0    DC    X'A1AEAFFF'                                                      
SYSF1    DC    X'A1AEAFFF'                                                      
SYSF2    DC    X'A1AEAFFF'                                                      
SYSF3    DC    X'A1AEAFFF'                                                      
SYSF4    DC    X'A1AEAFFF'                                                      
SYSF5    DC    X'A1AEAFFF'                                                      
SYSF6    DC    X'A1AEAFFF'                                                      
SYSF7    DC    X'A1AEAFFF'                                                      
SYSF8    DC    X'A1AEAFFF'                                                      
SYSF9    DC    X'A1AEAFFF'                                                      
SYSFA    DC    X'A1AEAFFF'                                                      
SYSFB    DC    X'A1AEAFFF'                                                      
SYSFC    DC    X'A1AEAFFF'                                                      
SYSFD    DC    X'A1AEAFFF'                                                      
SYSFE    DC    X'A1AEAFFF'                                                      
SYSFF    DC    X'A1AEAFFF'                                                      
*&&                                                                             
         EJECT                                                                  
*                                                                               
*&&UK                                                                           
SYSSSER  DC    X'01A101AE01AF'                                                  
*                                                                               
SYSSMED  DC    X'04A1'                                                          
         DC    X'10A1'                                                          
         DC    X'11A1'                                                          
         DC    X'12A1'                                                          
         DC    X'13A1'                                                          
         DC    X'14A1'                                                          
         DC    X'15A1'                                                          
         DC    X'20A1'                                                          
         DC    X'21A1'                                                          
         DC    X'22A1'                                                          
         DC    X'23A1'                                                          
         DC    X'24A1'                                                          
         DC    X'25A1'                                                          
         DC    X'40A1'                                                          
         DC    X'41A1'                                                          
         DC    X'42A1'                                                          
         DC    X'43A1'                                                          
         DC    X'44A1'                                                          
         DC    X'45A1'                                                          
         DC    X'46A1'                                                          
         DC    X'47A1'                                                          
         DC    X'48A1'                                                          
         DC    X'49A1'                                                          
         DC    X'4AA1'                                                          
         DC    X'4BA1'                                                          
         DC    X'4CA1'                                                          
         DC    X'4DA1'                                                          
         DC    X'4EA1'                                                          
         DC    X'4FA1'                                                          
         DC    X'50A1'                                                          
         DC    X'51A1'                                                          
         DC    X'52A1'                                                          
         DC    X'53A1'                                                          
         DC    X'54A1'                                                          
         DC    X'55A1'                                                          
*                                                                               
SYSSMPL  DC    X'05A1'                                                          
         DC    X'1CA1'                                                          
         DC    X'64A1'                                                          
*                                                                               
SYSSACC  DC    X'06A1'                                                          
         DC    X'16A1'                                                          
         DC    X'17A1'                                                          
         DC    X'18A1'                                                          
         DC    X'19A1'                                                          
         DC    X'26A1'                                                          
         DC    X'27A1'                                                          
         DC    X'28A1'                                                          
         DC    X'29A1'                                                          
         DC    X'2AA1'                                                          
         DC    X'2BA1'                                                          
         DC    X'2CA1'                                                          
         DC    X'2DA1'                                                          
         DC    X'2EA1'                                                          
         DC    X'2FA1'                                                          
         DC    X'30A1'                                                          
         DC    X'31A1'                                                          
         DC    X'32A1'                                                          
         DC    X'33A1'                                                          
         DC    X'34A1'                                                          
         DC    X'35A1'                                                          
         DC    X'36A1'                                                          
         DC    X'37A1'                                                          
         DC    X'38A1'                                                          
         DC    X'39A1'                                                          
         DC    X'3AA1'                                                          
         DC    X'3BA1'                                                          
         DC    X'3CA1'                                                          
         DC    X'3DA1'                                                          
         DC    X'3EA1'                                                          
         DC    X'3FA1'                                                          
         DC    X'60A1'                                                          
         DC    X'66A1'                                                          
*                                                                               
SYSSFEE  DC    X'07A1'                                                          
         DC    X'1DA1'                                                          
         DC    X'61A1'                                                          
*                                                                               
SYSSMBA  DC    X'09A1'                                                          
         DC    X'1AA1'                                                          
*                                                                               
SYSSPER  DC    X'0EA1'                                                          
         DC    X'1EA1'                                                          
*                                                                               
         DC    X'0000'                                                          
*&&                                                                             
         EJECT                                                                  
QISFILE  EQU   X'01'                                                            
QISF20B  EQU   X'02'                                                            
QISFPDOV EQU   X'01'                                                            
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
SAVERD   DS    F                                                                
DMCB     DS    6F                                                               
RELO     DS    F                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
*                                                                               
FILEFLAG DS    X                                                                
FILE_RCV EQU   SFRCV               RECOVERY X'40'                               
FILE_REQ EQU   SFREQ               REQUEST  X'20'                               
FILE_DA  EQU   X'02'               DA FILE  X'02'                               
FILE_IS  EQU   SFISF               IS FILE  X'01'                               
*                                                                               
PARM     DS    6F                                                               
ASYSFACS DS    A                                                                
AHEXIN   DS    A                                                                
ADATAMGR DS    A                                                                
ALOCKSPC DS    A                                                                
AHEXOUT  DS    A                                                                
ASCANNER DS    A                                                                
AUNSCAN  DS    A                                                                
ATIOB    DS    A                                                                
ATIA     DS    A                                                                
AGETTXT  DS    A                                                                
DSPLINES DS    F                                                                
DSPINDEX DS    F                                                                
DSPLINDX DS    F                                                                
MAXXTNT  DS    F                                                                
APQBUFF  DS    A                                                                
CURSOR   DS    A                                                                
FMT      DS    X                                                                
FNDX     DS    X                                                                
FSUB     DS    X                                                                
FERRDSP  DS    X                                                                
THRDNUM  DS    X                                                                
STINDFLG DS    C                   Y=START INDEX WAS PROVIDED                   
XTRAMSGL DS    X                                                                
XTRAMSG  DS    CL25                                                             
GTB      DS    6F                                                               
*                                                                               
SYSINFO  DS    0CL52                                                            
SYS      DS    X                   SYS NUMBER (ZERO=ALL)                        
*&&US                                                                           
DEMO_SYS EQU   X'0C'               DEMO SYSTEM                                  
*&&                                                                             
SYSNAME  DS    CL7                 SYS NAME                                     
SYSADDR  DS    A                   SYS SYSFLES ADDR                             
SYSINDEX DS    F                   SYS DISPLAY INDEX                            
XTNINDEX DS    F                   XTN DISPLAY INDEX                            
FIL      DS    X                   SYS FILE NUMBER (ZERO=ALL)                   
FILNAME  DS    CL7                 SYS FILE NAME                                
FILADDR  DS    A                   SYS FILE SYSFLES ADDR                        
FILTYPE  DS    X                   SYS FILE TYPE FILTER                         
FILFMT   DS    X                   SYS FILE FORMAT                              
FILSYSNO DS    XL2                 SYS FILE SYSNO/UNIT FILTER                   
FILPCT   DS    F                   SYS FILE THRESHOLD PERCENTAGE                
PRTUSER  DS    XL2                 PRT USER ID NUM                              
PRTID    DS    CL3                 PRT REPORT ID                                
PRTCLASS DS    C                   PRT CLASS                                    
PRTREPNO DS    XL2                 PRT REPORT NUMBER ASSIGNED                   
FLXNTS   DS    XL1                 SYS # OF EXTENTS                             
         DS    CL3                 N/D                                          
                                                                                
FILINFO  DS    0CL80                                                            
FTYPE    DS    X                   FILE TYPE                                    
FTYPE_NP EQU   SFNOP               FILE NOP      X'80' (RESET AFTER)            
FTYPE_DX EQU   X'80'               DANDX FILE    X'80'                          
FTYPE_NF EQU   SFNOEOF             NO EOF SEARCH X'04'                          
FTYPE_IS EQU   SFISF               IS FILE       X'01'                          
FFLAGS   DS    XL2                 FILE FLAGS                                   
FFLG1_VS EQU   SFVSM               VSAM FILE     X'01' (WITH SFISF)             
FNUM     DS    X                   FILE NUMBER                                  
FDTF     DS    A                   FILE DTF ADDR                                
FNAME    DS    CL8                 FILE NAME                                    
FTRKS    DS    H                   FILE TRACKS/CYLINDER                         
FXNTS    DS    H                   FILE NUMBER OF EXTENTS                       
FLAST    DS    F                   FILE LAST MAIN REC                           
FINDX    DS    F                   FILE LAST INDEX REC...........IS             
FOFLO    DS    F                   FILE LAST OVERFLOW REC........IS             
FSIZE    DS    F                   FILE TOTAL SIZE                              
FAVAIL   DS    F                   FILE AVAIL SIZE                              
FPCT     DS    F                   FILE PERCENTAGE AVAILABLE                    
FISIZE   DS    F                   FILE INDEX EXTENT SIZE........IS             
FIAVAIL  DS    F                   FILE INDEX EXTENT AVAIL.......IS             
FIPCT    DS    F                   FILE INDEX PERCENTAGE AVAIL...IS             
FICSIZE  DS    F                   FILE INDEX CORE SIZE..........IS             
FICSIZEU DS    F                   FILE INDEX CORE AVAILABLE.....IS             
FICPCT   DS    F                   FILE INDEX CORE AVAIL PCT.....IS             
FICADDR  DS    F                   FILE INDEX ADDRESS............IS             
FICFLAG  DS    X                   FILE INDEX CORE FLAGS.........IS             
FITFLAG  DS    X                   FILE TYPE FLAG X'01' NEW......IS             
FDEV     DS    X                   FILE DEVICE                                  
*                                                                               
FXINF    DS    X                   FILE EXTRA INFO                              
FX_RO    EQU   X'80'               READ-ONLY                                    
FX_NOP   EQU   X'40'               FILE IS NOP                                  
FX_OPEN  EQU   X'20'               FILE IS OPEN                                 
FXGLOBAL EQU   X'10'               GLOBAL                                       
FXOGLOBL EQU   X'08'               ON-LINE GLOBAL ONLY                          
FX_QUI   EQU   X'04'               FILE IS QUIESCED                             
FX20BIT  EQU   X'02'               20-BIT TRACK                                 
FX18BIT  EQU   X'01'               18-BIT TRACK                                 
FX22BIT  EQU   X'03'               22-BIT TRACK                                 
*                                                                               
         DS    XL4                 N/D                                          
RW       DS    C                   +=UPDATIVE SYSTEMS ONLY (=XTNT,+)            
FSTLINE  DS    X                                                                
*                                                                               
P        DS    XL1                 KEEP WITH PLINE                              
PLINE    DS    CL132                                                            
*                                                                               
MSG      DS    CL60                                                             
WORK     DS    CL20                                                             
*                                                                               
HIGHXNT  DS    X                   HIGH VALUE FOR OPTION                        
XNTMAX   EQU   48                                                               
XNTINFO  DS    (XNTMAX)CL(XNTINFOQ)  EXTENT STORAGE AREAS                       
         DS    XL4                                                              
*                                                                               
IO       DS    2048X                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
XNTINFOD DSECT                                                                  
XNUM     DS    X                   EXTENT NUMBER                                
XSYS     DS    XL2                 EXTENT SYMBOLIC UNIT OR CHAN/UNIT            
XFLAG    DS    X                   EXTENT FLAGS                                 
XLOW     DS    F                   EXTENT CCHH LOW                              
XHIGH    DS    F                   EXTENT CCHH HIGH                             
XSIZE    DS    F                   EXTENT SIZE                                  
XAVAIL   DS    F                   EXTENT AVAILABLE                             
XINUSE   DS    F                   EXTENT INUSE                                 
XINDEX   DS    F                   EXTENT INDEX                                 
XLAST    DS    F                   EXTENT LAST RECORD                           
XMAX     DS    F                   EXTENT MAX RELATIVE TRACK                    
XNTINFOQ EQU   *-XNTINFOD          L'TABLE ENTRY                                
*                                                                               
XNTDISPD DSECT ,                   EXTENT DISPLAY LINE                          
XDDATA   DS    0CL79                                                            
*                                                                               
XDNAME   DS    CL8                                                              
         DS    CL1                                                              
XDPCT    DS    CL3                                                              
         DS    CL1                                                              
XDCHU    DS    CL7                                                              
         DS    CL1                                                              
XDCCHH   DS    CL11                                                             
         DS    CL1                                                              
XDSIZE   DS    CL5                                                              
         DS    CL1                                                              
*                                                                               
XDMIDCHR DS    CL1                                                              
*                                                                               
XDINUSE  DS    CL5                                                              
         DS    CL1                                                              
XDAVAIL  DS    CL5                                                              
         DS    CL1                                                              
*                                                                               
         ORG   XDCHU                                                            
XDB#XTNT DS    CL2                                                              
         DS    CL1                                                              
XDBSIZE  DS    CL11                                                             
         DS    CL1                                                              
XDBINUSE DS    CL11                                                             
         DS    CL1                                                              
XDBAVAIL DS    CL11                                                             
         DS    CL1                                                              
         ORG                                                                    
*                                                                               
XDFILE   DS    0CL5                                                             
XDFSTAT  DS    CL1                 OPEN STATUS                                  
XDFGLOB  DS    CL1                 GLOBAL                                       
         DS    CL1                 N/D                                          
XDFSIZE  DS    CL2                 SIZE OF FILE                                 
*                                                                               
         DS    CL1                                                              
XDLAST   DS    CL8                                                              
         DS    CL1                                                              
XDDTF    DS    CL6                                                              
         DS    CL1                                                              
XDDEV    DS    CL4                                                              
         DS    CL1                                                              
XNTDISPL EQU   *-XNTDISPD                                                       
         EJECT                                                                  
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
                                                                                
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
                                                                                
* DMDTFPH                                                                       
       ++INCLUDE DMDTFPH                                                        
                                                                                
* DMXTNTD                                                                       
       ++INCLUDE DMXTNTD                                                        
                                                                                
* DMSYSFD                                                                       
       ++INCLUDE DMSYSFD                                                        
                                                                                
* DMPRTQL                                                                       
       ++INCLUDE DMPRTQL                                                        
                                                                                
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
                                                                                
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
* DDSCANBLKD                                                                    
       ++INCLUDE DDSCANBLKD                                                     
                                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
                                                                                
* FAGETTXTD                                                                     
       ++INCLUDE FAGETTXTD                                                      
                                                                                
* DDFH                                                                          
       ++INCLUDE DDFH                                                           
                                                                                
SRXNTFFD DSECT                                                                  
         DS    CL64                                                             
* SRXNTFFD                                                                      
       ++INCLUDE SRXNTFFD                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SRXNT00   03/26/19'                                      
         END                                                                    
