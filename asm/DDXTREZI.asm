*          DATA SET DDXTREZI   AT LEVEL 004 AS OF 10/05/05                      
*PHASE XTREZIA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*                                                                               
XTREZI   CSECT                                                                  
         TITLE ' EXTRACT EZ INVOICES '                                          
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,*XTREZI*,=V(REGSAVE),R8,R9                                     
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         BRAS  RE,READCRDS                                                      
*                                                                               
         MVC   ORRECTYP,SPUIQ      UNKNOWN INVOICE REC TYPE C'07199'            
         MVI   ORACTION,C'L'       ACTION = L FOR LOAD                          
         GOTO1 =V(DATCON),DMCB,(5,0),(20,WORK)    TODAY(YYYYMMDD)               
         MVC   ORDATE,WORK                                                      
*                                                                               
         OPEN  (EZIFILE,INPUT)                                                  
         OPEN  (XTRFILE,OUTPUT)                                                 
*                                                                               
NEXT     GET   EZIFILE,REC                                                      
*                                                                               
         CLC   =C'99',REC+4                                                     
         BNE   XT05                                                             
*                                  RESET ALL OUTPUT FIELDS                      
         MVC   ORSOURCE,BLANKS                                                  
         MVC   ORLDATE,BLANKS                                                   
         MVC   ORIMAGN,BLANKS                                                   
         MVC   ORAGNAME,BLANKS                                                  
         MVC   ORAGADR1,BLANKS                                                  
         MVC   ORAGADR2,BLANKS                                                  
         MVC   ORAGADR3,BLANKS                                                  
         MVC   ORAGADR4,BLANKS                                                  
         MVC   ORSTNCL,BLANKS                                                   
         MVC   ORSTMED,BLANKS                                                   
         MVC   ORSTBAND,BLANKS                                                  
         MVC   ORIVAN,BLANKS                                                    
         MVC   ORIVNUM,BLANKS                                                   
         MVC   ORIVBMON,BLANKS                                                  
         B     XT20                                                             
*                                                                               
XT05     CLC   =C'34',REC+4                                                     
         BNE   XT20                                                             
*                                                                               
         OC    DATEFLT,DATEFLT     ALL DATE?                                    
         BZ    XT10                YES                                          
*                                                                               
         CLC   ORLDATE,DATEFLT     SAME DATE OR EARLIER?                        
         BH    XT12                NO - SKIP THIS RECORD                        
*                                                                               
XT10     PUT   XTRFILE,OUTREC                                                   
         MVC   P,OUTREC                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
XT12     EQU   *                   CLEAR INVOICE INFO <31>                      
         MVC   ORIVAN,BLANKS                                                    
         MVC   ORIVNUM,BLANKS                                                   
         MVC   ORIVBMON,BLANKS                                                  
*                                                                               
XT20     EQU   *                                                                
         USING XTRTABD,R4                                                       
         LA    R4,XTRTAB                                                        
XT50     CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    XT99X                                                            
*                                                                               
         CLC   XTRTTYP,REC+4       MATCH ON RECORD TYPE?                        
         BNE   XT90                                                             
*                                                                               
         LA    R2,REC+4                                                         
         ICM   R3,15,XTRFLDN       FIELD #                                      
         BRAS  RE,GETFLD           GET THIS FIELD VALUE                         
*                                                                               
         OC    XTRCNT,XTRCNT       ANY CONVERSION ROUTINE?                      
         BZ    XT70                                                             
         ICM   RF,15,XTRCNT                                                     
         BASR  RE,RF                                                            
*                                                                               
XT70     ICM   R2,15,XTROADR       A(OUTPUT FIELD)                              
         SR    R1,R1                                                            
         ICM   R1,1,XTROLEN        LENGTH OF OUTPUT FIELD                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FIELD                                                    
*                                                                               
         OC    XTROXTR,XTROXTR     ANY EXTRA PROCESS ROUTINE?                   
         BZ    XT90                                                             
         ICM   RF,15,XTROXTR                                                    
         BASR  RE,RF                                                            
*                                                                               
XT90     AHI   R4,L'XTRTAB                                                      
         B     XT50                                                             
         DROP  R4                                                               
XT99X    EQU   *                                                                
*                                                                               
         B     NEXT                                                             
*                                                                               
CLOSE    EQU   *                                                                
         CLOSE (EZIFILE)                                                        
         CLOSE (XTRFILE)                                                        
*                                                                               
         EJECT                                                                  
EXIT     XBASE                                                                  
         EJECT                                                                  
*                                                                               
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* DATE=BEFORE_TODAY   DEFAULT (ANY DATE BEFORE TODAY)                           
* DATE=ALL            ANY DATE                                                  
* DATE=YYYYMMDD       THIS DATE AND BEFORE                                      
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    RCX                                                              
*                                                                               
         CLC   =C'DATE=ALL',CARD         ALL DATE?                              
         BNE   RC20                                                             
         XC    DATEFLT,DATEFLT                                                  
         B     RCX                                                              
*                                                                               
RC20     CLC   =C'DATE=BEFORE_TODAY',CARD    BEFORE TODAY?                      
         BNE   RC30                                                             
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),TODAY6         TODAY(YYMMDD)               
         LHI   RE,-1                                                            
         ST    RE,DMCB+8                                                        
         GOTO1 =V(ADDAY),DMCB,TODAY6,WORK           YESTERDAY(YYMMDD)           
*                                                                               
         GOTO1 =V(DATCON),DMCB,WORK,(20,DATEFLT)    DATEFLT(YYYYMMDD)           
         B     RCX                                                              
*                                                                               
RC30     CLC   =C'DATE=',CARD                                                   
         BNE   RCX                                                              
         MVC   DATEFLT,CARD+5                                                   
*                                                                               
RCX      EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GETFLD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*EXTRACT THE DATA IN THE RECORD AT THE SPECIFIC FIELD #.                        
*NTRY: R2=A(REC), R3=FIELD# (FIRST FIELD IS FIELD #1)                           
*EXIT: RETURN VALUE IN "FIELD"(CL256), SPACES PADDED.                           
*      RETURN # OF BYTE IN "FIELDL"(F)                                          
*                                                                               
         XC    FIELDL,FIELDL       RESET FIELD LENGTH                           
         MVI   FIELD,C' '          SPACES PADDED "FIELD"                        
         MVC   FIELD+1(L'FIELD-1),FIELD                                         
*                                                                               
GF20     SR    R1,R1                                                            
GF30     CLI   0(R2),X'15'         END OF THE LINE?                             
         BE    GFX                 THIS FIELD IS NOT THERE                      
         CLI   0(R2),X'5E'         SEMICOLON?                                   
         BE    GF40                                                             
         AHI   R2,1                                                             
         AHI   R1,1                                                             
         B     GF30                                                             
*                                                                               
GF40     AHI   R2,1                PASS SEMICOLON                               
         AHI   R1,1                INCLUDE SEMICOLON IN LENGTH                  
         BCT   R3,GF20             FIND FIELD #, NO, NEXT FIELD                 
*                                                                               
         SR    R2,R1               PT R2 TO BEGINNING OF FIELD                  
         BCTR  R1,0                - LAST SEMICOLON                             
         ST    R1,FIELDL           SAVE THE FIELD LENGTH                        
         LTR   R1,R1                                                            
         BZ    GFX                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FIELD(0),0(R2)      MOVE IN FIELD                                
*                                                                               
         OC    FIELD,BLANKS                                                     
*                                                                               
GFX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
OVR21AGY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*IF 76 RECORD'S AGENCY NAME IS PRESENT,                                         
*OVERRIDE 21 RECORD'S AGENCY NAME W/ 76 RECORD'S AGENCY NAME                    
*                                                                               
         CLC   ORIMAGN,BLANKS                                                   
         BNH   *+10                                                             
         MVC   ORAGNAME,ORIMAGN                                                 
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CNVDATE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*NTRY: FIELD=DATE(YYMMDD)                                                       
*EXIT: FIELD=DATE(MMDDYYYY), SET FIELDL=8                                       
*                                                                               
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,FIELD),(20,WORK)      YYYYMMDD                
         MVC   FIELD(4),WORK+4                                                  
         MVC   FIELD+4(4),WORK                          MMDDYYYY                
         MVC   FIELDL,=F'8'                                                     
*                                                                               
CDX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
XTRTAB   DS    XL(XTRTLENQ)                                                     
         DC    CL2'99',AL4(2),AL4(0),AL1(L'ORSOURCE),AL4(ORSOURCE)              
         DC    AL4(0)                                                           
         DC    CL2'99',AL4(3),AL4(CNVDATE),AL1(L'ORLDATE),AL4(ORLDATE)          
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'76',AL4(2),AL4(0),AL1(L'ORIMAGN),AL4(ORIMAGN)                
         DC    AL4(OVR21AGY)                                                    
*                                                                               
         DC    CL2'21',AL4(3),AL4(0),AL1(L'ORAGNAME),AL4(ORAGNAME)              
         DC    AL4(OVR21AGY)                                                    
         DC    CL2'21',AL4(4),AL4(0),AL1(L'ORAGADR1),AL4(ORAGADR1)              
         DC    AL4(0)                                                           
         DC    CL2'21',AL4(5),AL4(0),AL1(L'ORAGADR2),AL4(ORAGADR2)              
         DC    AL4(0)                                                           
         DC    CL2'21',AL4(6),AL4(0),AL1(L'ORAGADR3),AL4(ORAGADR3)              
         DC    AL4(0)                                                           
         DC    CL2'21',AL4(7),AL4(0),AL1(L'ORAGADR4),AL4(ORAGADR4)              
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'22',AL4(2),AL4(0),AL1(L'ORSTNCL),AL4(ORSTNCL)                
         DC    AL4(0)                                                           
         DC    CL2'22',AL4(3),AL4(0),AL1(L'ORSTMED),AL4(ORSTMED)                
         DC    AL4(0)                                                           
         DC    CL2'22',AL4(4),AL4(0),AL1(L'ORSTBAND),AL4(ORSTBAND)              
         DC    AL4(0)                                                           
*                                                                               
         DC    CL2'31',AL4(4),AL4(0),AL1(L'ORIVAN),AL4(ORIVAN)                  
         DC    AL4(0)                                                           
         DC    CL2'31',AL4(9),AL4(0),AL1(L'ORIVNUM),AL4(ORIVNUM)                
         DC    AL4(0)                                                           
         DC    CL2'31',AL4(10),AL4(0),AL1(L'ORIVBMON),AL4(ORIVBMON)             
         DC    AL4(0)                                                           
         DC    X'FF'                                                            
*                                                                               
*                                                                               
EZIFILE  DCB   DDNAME=EZIFILE,DSORG=PS,MACRF=GM,EODAD=CLOSE                     
XTRFILE  DCB   DDNAME=XTRFILE,DSORG=PS,MACRF=PM                                 
*                                                                               
SAVE     DS    20F                                                              
DMCB     DS    6F                                                               
WORK     DS    CL20                                                             
DATEFLT  DS    CL8                 YYYYMMDD                                     
TODAY6   DS    CL6                 YYMMDD                                       
CARD     DS    CL80                                                             
READTHIS DS    C                                                                
FIELDL   DS    F                                                                
FIELD    DS    CL256                                                            
BLANKS   DC    CL256' '                                                         
REC      DS    CL2048                                                           
OUTREC   DS    CL2048                                                           
         ORG   OUTREC                                                           
         DC    AL2(ORRLENQ)        RECORD LENGTH                                
         DC    XL2'0000'           X'0000'                                      
ORRECTYP DS    CL5                 SPOT UNKNOWN INVOICE RECORD TYPE             
         DC    AL1(DELIM)                                                       
ORACTION DS    CL1                 ACTION (A/C/D/L)                             
         DC    AL1(DELIM)                                                       
ORDATE   DS    CL8                 ACTION DATE YYYYMMDD                         
         DC    AL1(DELIM)                                                       
ORSOURCE DS    CL4                 EZI SOURCE <99>                              
         DC    AL1(DELIM)                                                       
ORLDATE  DS    CL8                 EZI LOAD DATE (MMDDYYYY) <99>                
         DC    AL1(DELIM)                                                       
ORSTNCL  DS    CL10                EZI STATION CALL LETTER <22>                 
         DC    AL1(DELIM)                                                       
ORSTMED  DS    CL5                 EZI STATION MEDIA TYPE <22>                  
         DC    AL1(DELIM)                                                       
ORSTBAND DS    CL5                 EZI STATION BAND <22>                        
         DC    AL1(DELIM)                                                       
ORAGNAME DS    CL40                EZI AGENCY NAME <21>                         
         DC    AL1(DELIM)                                                       
ORAGADR1 DS    CL30                EZI AGENCY ADDRESS LINE 1 <21>               
         DC    AL1(DELIM)                                                       
ORAGADR2 DS    CL30                EZI AGENCY ADDRESS LINE 2 <21>               
         DC    AL1(DELIM)                                                       
ORAGADR3 DS    CL30                EZI AGENCY ADDRESS LINE 3 <21>               
         DC    AL1(DELIM)                                                       
ORAGADR4 DS    CL30                EZI AGENCY ADDRESS LINE 4 <21>               
         DC    AL1(DELIM)                                                       
ORIVAN   DS    CL25                EZI ADVERTISER NAME <31>                     
         DC    AL1(DELIM)                                                       
ORIVNUM  DS    CL12                EZI INVOICE NUMBER <31>                      
         DC    AL1(DELIM)                                                       
ORIVBMON DS    CL4                 EZI BROADCAST MONTH <31>                     
         DC    AL1(DELIM)                                                       
ORIMAGN  DS    CL40                EZI IM AGENCY NAME <76>                      
         DC    AL1(DELIM)                                                       
ORRLENQ  EQU   *-OUTREC                                                         
         ORG                                                                    
DELIM    EQU   X'5E'               SEMICOLON                                    
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPXRECID                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*                                                                               
XTRTABD  DSECT                                                                  
XTRTTYP  DS    CL2                 RECORD TYPE                                  
XTRFLDN  DS    AL4                 FIELD NUMBER                                 
XTRCNT   DS    AL4                 A(CONVERSION ROUNTINE)                       
XTROLEN  DS    AL1                 OUTPUT FIELD LENGTH                          
XTROADR  DS    AL4                 A(OUTPUT FIELD)                              
XTROXTR  DS    AL4                 A(EXTRA PROCESS ROUNTINE)                    
XTRTLENQ EQU   *-XTRTABD                                                        
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDXTREZI  10/05/05'                                      
         END                                                                    
